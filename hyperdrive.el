;;; hyperdrive.el --- P2P filesystem in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joseph Turner <joseph@ushin.org>

;; Author: Joseph Turner
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <joseph@ushin.org>
;; Created: 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (map "3.0") (compat "29.1.3.2") (plz "0.6-pre") (persist "0.5"))
;; Homepage: https://git.sr.ht/~ushin/hyperdrive.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; hyperdrive.el integrates with `hyper-gateway' for sharing files on the
;; https://hypercore-protocol.org network.

;;;; Installation:

;; `hyperdrive.el` is available on
;; [MELPA](https://melpa.org/#/getting-started).  Once you've set up
;; MELPA, you can run `M-x package-install` then enter `hyperdrive`.

;; `hyperdrive.el` relies on
;; [hyper-gateway](https://github.com/RangerMauve/hyper-gateway/) for
;; talking to the hypercore network (installation instructions -
;; <https://github.com/RangerMauve/hyper-gateway#how-do-i-install-hyper-gateway>).

;;; Code:

(require 'cl-lib)

(require 'bookmark)
(require 'json)
(require 'rx)

;; NOTE: On older versions of Emacs with older built-in versions of
;; `map', it's not possible to force Emacs to load the newer version
;; of the library when compiling the package; we can only
;; "Package-Requires" it to cause it to be installed.  So in the case
;; that a user encounters an error caused by macro-expanding an older
;; version of the library, the user would need to delete the
;; hyperdrive package, restart Emacs, and reinstall this package to
;; fix it.
(require 'map)

(require 'compat)
(require 'plz)
(require 'persist)

(require 'hyperdrive-lib)
(require 'hyperdrive-handlers)
(require 'hyperdrive-org)

(defgroup hyperdrive nil
  "Emacs gateway to the Hypercore network."
  :group 'communication
  :group 'external
  :prefix "hyperdrive-")

;;;; Configuration

(defcustom hyperdrive-storage-location
  (expand-file-name "~/.local/share/hyper-gateway-nodejs/")
  "Location to store Hypercore data."
  :type '(file :must-match t))

(defcustom hyperdrive-hyper-gateway-port 4973
  "Port on which to run the hyper-gateway server."
  :type 'natnum)

(defcustom hyperdrive-hyper-gateway-p2p-port 4977
  "Port on which to run the p2p network."
  :type 'natnum)

(defcustom hyperdrive-honor-auto-mode-alist t
  "If non-nil, use file extension of hyperdrive file to set `major-mode'."
  :type 'boolean)

(defcustom hyperdrive-persist-location nil
  "Location where `persist' will store data."
  :type '(choice (const :tag "Use default persist location" nil)
                 (file :tag "Custom location")))

(defcustom hyperdrive-download-directory (expand-file-name
                                          (if (bound-and-true-p eww-download-directory)
                                              (if (stringp eww-download-directory)
                                                  eww-download-directory
                                                (funcall eww-download-directory))
                                            "~/"))
  "Location where `hyperdrive-download-url' will download files.
Defaults to `eww-download-directory'."
  :type '(file :must-match t))

(defcustom hyperdrive-timestamp-format "%x %X"
  "Format string used for timestamps.
Passed to `format-time-string', which see."
  :type 'string
  :set (lambda (option value)
         (set option value)
         (setf hyperdrive-timestamp-format-string
               (format "%%%ss"
                       (string-width (format-time-string value))))))

(defcustom hyperdrive-directory-display-buffer-action
  '(display-buffer-same-window)
  "Display buffer action for hyperdrive directories.
Passed to `display-buffer', which see."
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "Pop up window" (display-buffer-pop-up-window))
                 (sexp :tag "Other")))

(defcustom hyperdrive-default-host-format
  '(petname nickname domain seed short-key public-key)
  "Default format for displaying hyperdrive hostnames.
Each option is checked in order, and the first available type is
used."
  :type '(repeat
          (choice (const :tag "Petname" petname)
                  (const :tag "Nickname"
                         :doc "(Nickname specified by hyperdrive author)"
                         :format "%t %h"
                         nickname)
                  (const :tag "DNSLink domain" domain)
                  (const :tag "Seed" seed)
                  (const :tag "Shortened public key" short-key)
                  (const :tag "Full public key" public-key))))

(defcustom hyperdrive-stream-player-command "mpv --force-window=immediate %s"
  "Command used to play streamable URLs externally.
In the command, \"%s\" is replaced with the URL (it should not be
quoted, because the arguments are passed directly rather than
through a shell)."
  :type '(choice (const :tag "MPV" "mpv --force-window=immediate %s")
                 (const :tag "VLC" "vlc %s")
                 (string :tag "Other command")))

(defcustom hyperdrive-queue-size 2
  "Default size of request queues."
  ;; TODO: Use this elsewhere also.
  :type 'integer)

;;;;; Faces

(defface hyperdrive-petname '((t :inherit font-lock-type-face))
  "Applied to hyperdrive petnames.")

(defface hyperdrive-seed '((t :inherit font-lock-doc-face))
  "Applied to hyperdrive seeds.")

(defface hyperdrive-domain '((t :inherit font-lock-keyword-face))
  "Applied to hyperdrive domains.")

(defface hyperdrive-nickname '((t :inherit font-lock-warning-face))
  "Applied to hyperdrive nicknames.")

(defface hyperdrive-public-key '((t :inherit font-lock-function-name-face))
  "Applied to hyperdrive public keys.")

;;;; Internal variables

;; NOTE: `persist' currently does not work correctly with hash tables
;; if the default value of a persisted variable is one; it considers
;; them equal at save time and so deletes the persisted variable file.
;; To work around this, we set the default value to nil and initialize
;; it to a hash table "manually".
;; TODO: See persist.el patch: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=63513>
(persist-defvar hyperdrive-hyperdrives nil
                "List of known hyperdrives."
                hyperdrive-persist-location)
(unless hyperdrive-hyperdrives
  (setf hyperdrive-hyperdrives (make-hash-table :test #'equal)))

;; TODO: Flesh out the persist hook.
;; (defvar hyperdrive-persist-hook nil
;;   :type 'hook)

;;;; Links

(defvar browse-url-handlers)
(defvar thing-at-point-uri-schemes)

(defun hyperdrive-browse-url (url &rest _ignore)
  "Browse hyperdrive URL."
  (hyperdrive-open-url url))

(when (version<= "28.1" emacs-version)
  (require 'browse-url)
  (require 'thingatpt)

  (cl-pushnew (cons (rx bos "hyper://") #'hyperdrive-browse-url)
              browse-url-handlers :test #'equal)
  (cl-pushnew "hyper://" thing-at-point-uri-schemes :test #'equal))

;;;; Commands

;; TODO(A): Command to rename paths.

(defvar hyperdrive-describe-current-hyperdrive)

(defun hyperdrive-describe-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `hyperdrive-describe-mode' buffer.
Gets latest metadata from hyperdrive."
  (hyperdrive-fill-metadata hyperdrive-describe-current-hyperdrive)
  (hyperdrive-describe-hyperdrive hyperdrive-describe-current-hyperdrive))

(define-derived-mode hyperdrive-describe-mode special-mode
  `("Hyperdrive-describe"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for buffers for describing hyperdrives."
  :interactive nil
  (setq-local revert-buffer-function #'hyperdrive-describe-revert-buffer))

(defun hyperdrive-describe-hyperdrive (hyperdrive)
  "Display various information about HYPERDRIVE.

Prefix argument forces `hyperdrive-complete-hyperdrive' to prompt
for a hyperdrive."
  ;; TODO: Display latest known version of hyperdrive? Should we
  ;; store/persist that info in the hyperdrive struct?
  (interactive (list (hyperdrive-complete-hyperdrive :force-prompt current-prefix-arg)))
  (with-current-buffer (get-buffer-create
                        (format "*Hyperdrive: %s*"
                                (hyperdrive--format-host hyperdrive :format '(short-key)
                                                         :with-label t)))
    (hyperdrive-describe-mode)
    (setq-local hyperdrive-describe-current-hyperdrive hyperdrive)
    (pcase-let (((cl-struct hyperdrive metadata domains writablep) hyperdrive)
                (inhibit-read-only t))
      (erase-buffer)
      (insert
       (propertize "Hyperdrive: \n" 'face 'bold)
       (format "Public key: %s\n" (hyperdrive--format-host hyperdrive :format '(public-key)))
       (format "Seed: %s\n" (or (hyperdrive--format-host hyperdrive :format '(seed))
                                "[none]"))
       (format "Petname: %s\n" (or (hyperdrive--format-host hyperdrive :format '(petname))
                                   "[none]"))
       (format "Nickname: %s\n" (or (hyperdrive--format-host hyperdrive :format '(nickname))
                                    "[none]"))
       (format "Domains: %s\n"
               (if domains
                   (string-join (mapcar (lambda (domain)
                                          (propertize domain 'face 'hyperdrive-domain))
                                        domains)
                                ", ")
                 "[none]"))
       (format "Writable: %s\n" (if writablep "yes" "no"))
       ;; TODO: Consider removing metadata table since we already display nickname above
       (format "Metadata: %s\n"
               (if metadata
                   (with-temp-buffer
                     (require 'org)
                     (org-mode)
                     (insert "\n|-\n| Key | Value |\n|-\n")
                     (cl-loop for (key . value) in metadata
                              do (insert (format "| %s | %s |\n" key value)))
                     (insert "|-\n")
                     (forward-line -1)
                     (org-table-align)
                     (buffer-string))
                 "[none]"))))
    (setq buffer-read-only t)
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun hyperdrive-start ()
  "Start `hyper-gateway' systemd service if not already running."
  (interactive)
  ;; TODO: Verify that the latest version is installed.  See: <https://github.com/RangerMauve/hyper-gateway/issues/9>.
  (let ((buffer (get-buffer-create " *hyperdrive-start*")))
    (unwind-protect
        (unless (zerop (call-process "systemctl" nil (list buffer t) nil "--user" "start" "hyper-gateway.service"))
          (error "Unable to start hyper-gateway: %S"
                 (with-current-buffer buffer
                   (string-trim-right (buffer-string)))))
      (kill-buffer buffer))))

;;;###autoload
(defun hyperdrive-stop ()
  "Stop `hyper-gateway' systemd service."
  (interactive)
  (let ((buffer (get-buffer-create " *hyperdrive-stop*")))
    (unwind-protect
        (unless (zerop (call-process "systemctl" nil (list buffer t) nil "--user" "stop" "hyper-gateway.service"))
          (error "Unable to stop hyper-gateway: %S"
                 (with-current-buffer buffer
                   (string-trim-right (buffer-string)))))
      (kill-buffer buffer))))

(defun hyperdrive--gateway-active-p ()
  "Return non-nil if `hyper-gateway' systemd service is active."
  (zerop (call-process-shell-command "systemctl --user is-active hyper-gateway.service")))

;;;###autoload
(defun hyperdrive-status ()
  "Say whether `hyper-gateway' systemd service is running."
  (interactive)
  (if (zerop (call-process-shell-command "systemctl --user is-active hyper-gateway.service"))
      (hyperdrive-message "Gateway is running.")
    (hyperdrive-message "Gateway is not running.")))

;;;###autoload
(defun hyperdrive-hyper-gateway-version ()
  "Say version number of `hyper-gateway'.
Gateway must be running."
  (interactive)
  (condition-case err
      (let ((url (concat "http://localhost:" (number-to-string hyperdrive-hyper-gateway-port) "/")))
        (hyperdrive-message "hyper-gateway version %s"
                            (alist-get 'version (plz 'get url :as #'json-read))))
    ;; TODO: Consolidate plz error handling
    (plz-curl-error
     (if (equal 7 (car (plz-error-curl-error (caddr err))))
         (hyperdrive-message "hyper-gateway not running.  Use \"M-x hyperdrive-start RET\" to start it")
       (signal (car err) (cdr err))))))

;;;###autoload
(defun hyperdrive-new (seed)
  "Open new hyperdrive for SEED.

If SEED is not currently used as the petname for another
hyperdrive, the new hyperdrive's petname will be set to SEED."
  (interactive (list (hyperdrive-read-name :prompt "New hyperdrive seed")))
  (let* ((response (with-local-quit
                     (hyperdrive-api 'post (concat "hyper://localhost/?key=" (url-hexify-string seed)))))
         (url (progn
                ;; NOTE: Working around issue in plz whereby the
                ;; stderr process sentinel sometimes leaves "stderr
                ;; finished" garbage in the response body in older
                ;; Emacs versions.  See: <https://github.com/alphapapa/plz.el/issues/23>.
                (string-match (rx bos (group "hyper://" (1+ nonl))) response)
                (match-string 1 response)))
         (hyperdrive (hyperdrive-entry-hyperdrive (hyperdrive-url-entry url))))
    (setf (hyperdrive-seed hyperdrive) seed
          (hyperdrive-writablep hyperdrive) t)
    (unwind-protect
        (hyperdrive-set-petname seed hyperdrive)
      (hyperdrive-persist hyperdrive)
      (hyperdrive-open (hyperdrive-url-entry url)))))

(defun hyperdrive-set-petname (petname hyperdrive)
  "Set HYPERDRIVE's PETNAME.
Entering an empty or blank string unsets PETNAME.
Returns HYPERDRIVE.

Prefix argument forces `hyperdrive-complete-hyperdrive' to prompt
for a hyperdrive."
  (interactive
   (let* ((hyperdrive (hyperdrive-complete-hyperdrive :force-prompt current-prefix-arg))
          (petname (hyperdrive-read-name
                    :prompt (format "Petname for «%s» (leave blank to unset)"
                                    (hyperdrive--format-hyperdrive hyperdrive))
                    :initial-input (hyperdrive-petname hyperdrive))))
     (list petname hyperdrive)))
  (while-let (((not (equal petname (hyperdrive-petname hyperdrive))))
              (other-hyperdrive (cl-find petname (hash-table-values hyperdrive-hyperdrives)
                                         :key #'hyperdrive-petname :test #'equal)))
    (setf petname (hyperdrive-read-name
                   :prompt (format "%S already assigned as petname to hyperdrive «%s».  Enter new petname"
                                   petname (hyperdrive--format-hyperdrive other-hyperdrive))
                   :initial-input (hyperdrive-petname hyperdrive))))
  (if (string-blank-p petname)
      (when (yes-or-no-p (format "Unset petname for «%s»? "
                                 (hyperdrive--format-hyperdrive hyperdrive)))
        (setf (hyperdrive-petname hyperdrive) nil))
    (setf (hyperdrive-petname hyperdrive) petname))
  (hyperdrive-persist hyperdrive)
  ;; TODO: Consider refreshing buffer names, directory headers, etc.
  hyperdrive)

(defun hyperdrive-set-nickname (nickname hyperdrive)
  "Set HYPERDRIVE's NICKNAME.
Returns HYPERDRIVE.

Prefix argument forces `hyperdrive-complete-hyperdrive' to prompt
for a hyperdrive."
  (interactive
   (let* ((hyperdrive (hyperdrive-complete-hyperdrive :predicate #'hyperdrive-writablep
                                                      :force-prompt current-prefix-arg))
          (nickname
           ;; NOTE: Fill metadata first in case the JSON file has been updated manually
           (progn
             (hyperdrive-fill-metadata hyperdrive)
             (hyperdrive-read-name
              :prompt (format "Nickname for «%s»"
                              (hyperdrive--format-hyperdrive hyperdrive))
              :initial-input (alist-get 'name (hyperdrive-metadata hyperdrive))))))
     (list nickname hyperdrive)))
  (unless (equal nickname (alist-get 'name (hyperdrive-metadata hyperdrive)))
    (if (string-blank-p nickname)
        (progn
          (cl-callf map-delete (hyperdrive-metadata hyperdrive) 'name)
          (hyperdrive-put-metadata hyperdrive
            :then (lambda (&rest _) (hyperdrive-message "Unset nickname"))))
      (setf (alist-get 'name (hyperdrive-metadata hyperdrive)) nickname)
      (hyperdrive-put-metadata hyperdrive
        :then (lambda (&rest _)
                (hyperdrive-message "Set nickname for «%s» to %s"
                                    (hyperdrive--format-hyperdrive hyperdrive)
                                    (hyperdrive--format-host hyperdrive :format '(nickname))))))
    ;; TODO: Consider refreshing buffer names, directory headers, etc, especially host-meta.json entry buffer.
    (hyperdrive-persist hyperdrive))
  hyperdrive)

(defun hyperdrive-revert-buffer (&optional _ignore-auto noconfirm)
  "Revert `hyperdrive-mode' buffer by reloading hyperdrive contents."
  (when (or (hyperdrive--entry-directory-p hyperdrive-current-entry)
            noconfirm
            ;; TODO: Add option hyperdrive-revert-without-query ?
            ;; (and (not (buffer-modified-p))
            ;;      (catch 'found
            ;;        (dolist (regexp revert-without-query)
            ;;          (when (string-match regexp file-name)
            ;;            (throw 'found t)))))
            (yes-or-no-p
             (format (if (buffer-modified-p)
                         "Hyperdrive: Discard edits and reread from %s? "
                       "Hyperdrive: Revert buffer from %s? ")
                     (hyperdrive-entry-url hyperdrive-current-entry))))
    ;; TODO: Support before-revert-hook, after-revert-hook, revert-buffer-internal-hook
    ;; Setting the modified flag to nil prevents `hyperdrive-open'
    ;; from erroring if it has been modified.
    (set-buffer-modified-p nil)
    (hyperdrive-open hyperdrive-current-entry)
    t))

(defun hyperdrive-revert-buffer-quick ()
  "Like `revert-buffer-quick', but works with `hyperdrive-mode' files."
  (interactive nil hyperdrive-mode)
  (hyperdrive-revert-buffer nil (not (buffer-modified-p))))

;;;; hyperdrive-mode

;; TODO: Investigate possibility of not having `hyperdrive-mode', of
;; just using `hyperdrive-current-entry'.

(define-minor-mode hyperdrive-mode
  "Minor mode for buffers opened from hyperdrives."
  ;; TODO: When users change the major-mode inside a buffer visiting hyperdrive file,
  ;; `hyperdrive-mode' is disabled. We check for `hyperdrive-mode' inside a number of
  ;; hyperdrive-* functions. Should we instead check for `hyperdrive-current-entry'
  ;; and disregard the current value of `hyperdrive-mode'?
  :global nil
  :interactive nil
  :group 'hyperdrive
  :lighter " hyperdrive"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap revert-buffer-quick] #'hyperdrive-revert-buffer-quick)
            (define-key map [remap dired-jump]  #'hyperdrive-up)
            map)
  (if hyperdrive-mode
      (progn
        (setq-local revert-buffer-function #'hyperdrive-revert-buffer
                    bookmark-make-record-function #'hyperdrive-bookmark-make-record)
        (cl-pushnew #'hyperdrive--write-contents write-contents-functions))
    ;; FIXME: Use `kill-local-variable' for `revert-buffer-function'.
    (kill-local-variable 'bookmark-make-record-function)
    (setq-local revert-buffer-function #'revert-buffer--default
                write-contents-functions
                (remove #'hyperdrive--write-contents write-contents-functions))))

;;;###autoload
(defun hyperdrive-find-file (entry)
  "Find hyperdrive ENTRY.
Interactively, prompts for known hyperdrive and path.

With prefix argument, prompts for more information. See
`hyperdrive-read-entry' and `hyperdrive-complete-hyperdrive'."
  (interactive (list (hyperdrive-read-entry :force-prompt current-prefix-arg)))
  (hyperdrive-open entry))

;;;###autoload
(defun hyperdrive-open-url (url)
  "Open hyperdrive URL."
  (interactive (list (hyperdrive-read-url :prompt "Open hyperdrive URL")))
  (hyperdrive-open (hyperdrive-url-entry url)))

(cl-defun hyperdrive-open (entry &key then recurse)
  "Open hyperdrive ENTRY.
If RECURSE, proceed up the directory hierarchy if given path is
not found.  THEN may be a function to pass to the handler to call
in the buffer opened by the handler."
  (declare (indent defun))
  ;; TODO: Add `find-file'-like interface. See <https://todo.sr.ht/~ushin/ushin/16>
  ;; TODO: When possible, check whether drive is writable with a HEAD request, and set writablep in the
  ;; struct. If the hyperdrive already exists in hyperdrive-hyperdrives, there's no need to send a HEAD
  ;; request, since the value will never change. We only need to send a HEAD request when calling
  ;; `hyperdrive-open-url' on an unknown URL. Since `hyperdrive-complete-url' only returns a URL, we'll
  ;; need to parse the URL and then call `gethash' (or refactor `hyperdrive-complete-url').
  ;; See: <https://github.com/RangerMauve/hypercore-fetch/issues/60>. (implemented)
  ;; TODO: What happens if the user tries to open a hyperdrive file that's already open in a buffer?
  ;; FIXME: Some of the synchronous filling functions we've added now cause this to be blocking,
  ;; which is very noticeable when a file can't be loaded from the gateway and eventually times out.
  (let ((hyperdrive (hyperdrive-entry-hyperdrive entry)))
    (hyperdrive-fill entry
      :then (lambda (entry)
              (pcase-let* (((cl-struct hyperdrive-entry type) entry)
                           ;; MAYBE: Use alist-get instead of cl-find-if.
                           (handler (or (cdr (cl-find-if (lambda (regexp)
                                                           (string-match-p regexp type))
                                                         hyperdrive-type-handlers :key #'car))
                                        #'hyperdrive-handler-default)))
                (hyperdrive-persist hyperdrive)
                (funcall handler entry :then then)))
      :else (lambda (plz-error)
              (cl-labels ((go-up
                           () (if recurse
                                  (hyperdrive-open (hyperdrive-parent entry) :recurse t)
                                (pcase (prompt-to-go-up)
                                  (1 (hyperdrive-open (hyperdrive-parent entry)))
                                  (`t (hyperdrive-open (hyperdrive-parent entry) :recurse t)))))
                          (prompt-to-go-up
                           () (pcase-exhaustive
                                  (read-answer (format "URL not found: \"%s\".  Try to load parent directory? "
                                                       (hyperdrive-entry-url entry))
                                               '(("yes" ?y "go up one level")
                                                 ("no" ?n "exit")
                                                 ("recurse" ?! "go up until a directory is found")))
                                ("yes" 1)
                                ("recurse" t)
                                ("no" nil))))
                (pcase-let (((cl-struct plz-error curl-error response) plz-error))
                  (pcase curl-error
                    (`(7 . ,_message) ;; Connection fails, most likely the gateway isn't started yet.
                     (hyperdrive-message "hyper-gateway not running.  Use \"M-x hyperdrive-start RET\" to start it"))
                    (`(,curl-code . ,message) ;; Any other curl error.
                     (hyperdrive-message "curl error: %s: %S" curl-code message))
                    (_ ;; Any other error is an HTTP error.
                     (pcase (plz-response-status response)
                       (404 ;; Path not found.
                        (if (and (not (hyperdrive--entry-directory-p entry))
                                 (hyperdrive-writablep hyperdrive)
                                 (not (hyperdrive-entry-version entry)))
                            ;; Hyperdrive is not a directory, is writable
                            ;; and is not versioned: create a new buffer
                            ;; that will be saved to that path.
                            (if-let ((buffer (get-buffer (hyperdrive--entry-buffer-name entry))))
                                ;; Buffer already exists: likely the user deleted the entry
                                ;; without killing the buffer.  Switch to the buffer and
                                ;; alert the user that the entry no longer exists.
                                (progn
                                  (switch-to-buffer buffer)
                                  (message "Entry no longer exists!  %s" (hyperdrive-entry-description entry)))
                              ;; Make and switch to new buffer.
                              (switch-to-buffer (hyperdrive--get-buffer-create entry)))
                          ;; Hyperdrive entry is not writable: offer to go up.
                          (go-up)))
                       (_ (hyperdrive-message "Unable to load URL \"%s\": %S"
                                              (hyperdrive-entry-url entry) plz-error)))))))))))

;;;###autoload
(defun hyperdrive-download-entry (entry filename)
  "Download ENTRY to FILENAME on disk.
Interactively, downloads current hyperdrive file.  If current
buffer is not a hyperdrive file, prompts with
`hyperdrive-read-entry'.

With prefix argument, prompts for more information. See
`hyperdrive-read-entry' and `hyperdrive-complete-hyperdrive'."
  (interactive
   (pcase-let* ((entry (if hyperdrive-mode
                           hyperdrive-current-entry
                         (hyperdrive-read-entry :force-prompt current-prefix-arg)))
                ((cl-struct hyperdrive-entry name) entry)
                (read-filename (read-file-name "Filename: " (expand-file-name name hyperdrive-download-directory))))
     (list entry read-filename)))
  (hyperdrive-download-url (hyperdrive-entry-url entry) filename))

;;;###autoload
(defun hyperdrive-download-url (url filename)
  "Load contents at URL as a file to store on disk at FILENAME."
  ;; TODO: Handle directory URLs (recursively download contents?)
  (interactive
   (let* ((read-url (hyperdrive-read-url :prompt "Download hyperdrive URL"))
          (name (hyperdrive-entry-name (hyperdrive-url-entry read-url)))
          (read-filename (read-file-name "Filename: " (expand-file-name name hyperdrive-download-directory))))
     (list read-url read-filename)))
  (hyperdrive-api 'get url :as `(file ,filename)))

;;;###autoload
(defun hyperdrive-write-buffer (entry &optional overwritep)
  "Write current buffer to new hyperdrive ENTRY.
If file already exists and OVERWRITEP is nil, prompt the user to
overwrite.

With prefix argument, prompts for more information. See
`hyperdrive-read-entry' and `hyperdrive-complete-hyperdrive'."
  (interactive (list (hyperdrive-read-entry :predicate #'hyperdrive-writablep
                                            :force-prompt current-prefix-arg
                                            :default-path (when (and hyperdrive-current-entry
                                                                     (not current-prefix-arg))
                                                            (hyperdrive-entry-path hyperdrive-current-entry))
                                            :allow-version-p nil)))
  ;; FIXME: Overwrites without prompting if file exists.
  (ignore overwritep)
  (pcase-let (((cl-struct hyperdrive-entry name) entry)
              (url (hyperdrive-entry-url entry))
              (buffer (current-buffer)))
    (hyperdrive-write entry
      :body (save-restriction
              (widen)
              (buffer-substring-no-properties (point-min) (point-max)))
      :then (lambda (response)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (unless hyperdrive-mode
                    ;; TODO: Remove faces/overlays that might be applied to current buffer.
                    ;; I can confirm that overlays are not removed when
                    ;; `hyperdrive-write-buffer' is called from a magit log buffer.
                    (hyperdrive-mode))
                  (hyperdrive--fill entry (plz-response-headers response))
                  ;; PUT responses only include ETag and Last-Modified
                  ;; headers, so we need to set other entry metadata manually.
                  ;; FIXME: For large buffers, `buffer-size' returns a different
                  ;; value than hyper-gateway's Content-Length header.
                  (setf (hyperdrive-entry-size entry) (buffer-size))
                  ;; FIXME: Will entry type ever be anything besides text/plain?
                  (setf (hyperdrive-entry-type entry) "text/plain; charset=utf-8")
                  (setq-local hyperdrive-current-entry entry)
                  (setf buffer-file-name nil)
                  (rename-buffer (hyperdrive--entry-buffer-name entry) 'unique)
                  (set-buffer-modified-p nil)
                  ;; Update the visited file modtime so undo commands
                  ;; correctly set the buffer-modified flag.  We just
                  ;; use `current-time' here since it's good enough
                  ;; and lets us avoid making another request for
                  ;; metadata.
                  (set-visited-file-modtime (current-time))))
              (hyperdrive-message "Wrote: %S to \"%s\"" name url))
      :else (lambda (plz-error)
              (pcase-let* (((cl-struct plz-error response) plz-error)
                           ((cl-struct plz-response status) response)
                           (message
                            (pcase status
                              (403 "Hyperdrive not writable")
                              (405 "Cannot write to old version")
                              (_ plz-error))))
                (hyperdrive-message "Unable to write: %S: %S" name message))))
    (hyperdrive-message "Saving to \"%s\"..." url)
    ;; TODO: Reload relevant hyperdrive-dir buffers after writing buffer (if ewoc buffers display version, then possibly all ewoc buffers for a given hyperdrive should be reloaded)
    ))

(defun hyperdrive--write-contents ()
  "Call `hyperdrive-write-buffer' for the current buffer.
To be used in `write-contents-functions'."
  (cl-assert hyperdrive-mode)
  (hyperdrive-write-buffer hyperdrive-current-entry))

(defun hyperdrive-copy-url (entry)
  "Save hyperdrive ENTRY's URL to the kill ring.
Interactively, uses `hyperdrive-current-entry', from either a
hyperdrive directory listing or a `hyperdrive-mode' file buffer."
  (interactive (list hyperdrive-current-entry) hyperdrive-mode)
  (let ((url (hyperdrive-entry-url entry)))
    (kill-new url)
    (hyperdrive-message "%s" url)))

(defun hyperdrive-up ()
  "Go up to parent directory."
  (interactive nil hyperdrive-mode)
  (if-let ((parent (hyperdrive-parent hyperdrive-current-entry)))
      (hyperdrive-open parent)
    (user-error "At root directory")))

(defun hyperdrive-previous-version (entry)
  "Show previous version of ENTRY."
  (interactive (list hyperdrive-current-entry) hyperdrive-mode)
  (if-let ((previous-entry (hyperdrive-entry-previous entry)))
      (hyperdrive-find-file previous-entry)
    (hyperdrive-message "At earliest version of entry")))

;;;; Bookmark support

(require 'bookmark)

(defun hyperdrive-bookmark-make-record ()
  "Return a bookmark record for current hyperdrive buffer.
Works in `hyperdrive-mode' and `hyperdrive-dir-mode' buffers."
  (let ((bookmark (bookmark-make-record-default 'no-file)))
    (setf (alist-get 'handler bookmark) #'hyperdrive-bookmark-handler
          (alist-get 'location bookmark) (hyperdrive-entry-url hyperdrive-current-entry))
    (cons (format "hyperdrive: %s" (hyperdrive-entry-description hyperdrive-current-entry)) bookmark)))

(defun hyperdrive-bookmark-handler (bookmark)
  "Handler for Hyperdrive BOOKMARK."
  (hyperdrive-open (hyperdrive-url-entry (alist-get 'location (cdr bookmark)))
    :then (lambda ()
            (bookmark-default-handler
             ;; We add the buffer property, because we don't want to
             ;; store that in the bookmark record, because the buffer
             ;; name could change in the future, and that would make
             ;; the record invalid, which would cause
             ;; `bookmark-default-handler' to signal an error.
             (append bookmark `((buffer . ,(current-buffer))))))))

(defun hyperdrive-bookmark-jump (bookmark)
  "Jump to a Hyperdrive BOOKMARK."
  (interactive
   (progn
     (bookmark-maybe-load-default-file) ; paranoia
     (list
      (completing-read "Open Hyperdrive bookmark: " bookmark-alist
                       (pcase-lambda (`(,_name . ,(map handler)))
                         (equal handler #'hyperdrive-bookmark-handler))
                       t nil 'bookmark-history))))
  (bookmark-jump bookmark))

(defun hyperdrive-bookmark-list ()
  "List Hyperdrive bookmarks."
  (interactive)
  (let ((bookmark-alist
         (cl-remove-if-not (pcase-lambda (`(,_name . ,(map handler)))
                             (equal handler #'hyperdrive-bookmark-handler))
                           bookmark-alist)))
    (call-interactively #'bookmark-bmenu-list)))

;;;; Upload files from disk

(cl-defun hyperdrive-upload-file
    (filename entry &key queue
              (then (lambda (&rest _ignore)
                      (hyperdrive-open (hyperdrive-parent entry))
                      (hyperdrive-message "Uploaded: \"%s\"." (hyperdrive-entry-url entry)))))
  "Upload FILENAME to ENTRY.
Interactively, read FILENAME and ENTRY from the user.  When
QUEUE, use it.

With prefix argument, prompts for more information. See
`hyperdrive-read-entry' and `hyperdrive-complete-hyperdrive'."
  (declare (indent defun))
  (interactive (let ((filename (read-file-name "Upload file: ")))
                 (list filename
                       (hyperdrive-read-entry :predicate #'hyperdrive-writablep
                                              :default-path (file-name-nondirectory filename)
                                              :force-prompt current-prefix-arg
                                              :allow-version-p nil))))
  (let ((url (hyperdrive-entry-url entry)))
    (hyperdrive-api 'put url :queue queue
      :body `(file ,filename)
      :then then)
    (unless queue
      (hyperdrive-message "Uploading to \"%s\"..." url))))

;; TODO: Don't overwrite a hyperdrive file with the same
;; contents. Should we keep a cache of uploaded files and mtimes?

(cl-defun hyperdrive-mirror
    (source hyperdrive &key target-dir (predicate #'always) no-confirm)
  "Mirror SOURCE to TARGET-DIR in HYPERDRIVE.

Only mirror paths within SOURCE for which PREDICATE returns
non-nil.  PREDICATE may be a function, which receives the expanded
filename path as its argument, or a regular expression, which is
tested against each expanded filename path. SOURCE is a directory
name.

When TARGET-DIR is nil, SOURCE is mirrored into the
hyperdrive's root directory \"/\".

Opens the \"*hyperdrive-mirror*\" buffer with the list of files to
be uploaded and the URL at which each file will be published.  See
`hyperdrive-mirror-mode'.

When NO-CONFIRM is non-nil, upload without prompting.

Interactively, with one universal prefix, prompt for predicate,
otherwise mirror all files.  With two universal prefixes, prompt
for predicate and set NO-CONFIRM to t."
  (interactive
   (let ((source (read-directory-name "Mirror directory: " nil nil t))
         (hyperdrive (hyperdrive-complete-hyperdrive :predicate #'hyperdrive-writablep
                                                     :force-prompt t)))
     (list source hyperdrive
           :target-dir (hyperdrive-read-path :hyperdrive hyperdrive :prompt "Target directory in «%s»" :default "/")
           :no-confirm (equal '(16) current-prefix-arg)
           :predicate (if current-prefix-arg
                          (let* ((collection
                                  '(("Mirror all files" . (lambda ()
                                                            #'always))
                                    ("`rx' form" . (lambda ()
                                                     (eval (read--expression "`rx' form: " "(rx )"))))
                                    ("Regexp string" . (lambda ()
                                                         (read-regexp "Regular expression: ")))
                                    ("Lambda function" . (lambda ()
                                                           (read--expression "Lambda: " "(lambda (filename) )")))
                                    ("Named function" .
                                     (lambda ()
                                       (completing-read "Named function: "
                                                        (let ((functions))
                                                          (mapatoms (lambda (atom)
                                                                      (when (functionp atom)
                                                                        (push atom functions))))
                                                          functions))))))
                                 (choice (completing-read "Predicate type: " collection))
                                 (result (funcall (alist-get choice collection nil nil #'equal))))
                            result)
                        #'always))))
  (cl-callf expand-file-name source)
  (setf target-dir (hyperdrive--format-path target-dir :directoryp t))
  (when (stringp predicate)
    (let ((regexp predicate))
      (setf predicate (lambda (filename)
                        (string-match-p regexp filename)))))
  (let* ((files (cl-remove-if-not predicate (directory-files-recursively source ".")))
         (parent-entry (hyperdrive-make-entry :hyperdrive hyperdrive :path target-dir :encode t))
         (files-and-urls
          ;; Structured according to `tabulated-list-entries'
          (mapcar (lambda (file)
                    (let ((url (hyperdrive-entry-url
                                (hyperdrive-make-entry
                                 :hyperdrive hyperdrive
                                 :path (expand-file-name (file-relative-name file source) target-dir)
                                 :encode t))))
                      ;; TODO: Wrap the first url in `substring-no-properties' to reduce memory usage?
                      (list url (vector file url))))
                  files)))
    (unless files
      (user-error "No files selected for mirroring (double-check predicate)"))
    (if no-confirm
        (hyperdrive--mirror files-and-urls parent-entry)
      (pop-to-buffer (get-buffer-create "*hyperdrive-mirror*"))
      (hyperdrive-mirror-mode)
      (setq-local hyperdrive-mirror-already-uploaded nil)
      (setq-local hyperdrive-mirror-parent-entry parent-entry)
      (setf tabulated-list-entries files-and-urls)
      (tabulated-list-print))))

(defun hyperdrive--mirror (files-and-urls parent-entry)
  "Upload each file to its corresponding URL in FILES-AND-URLs.
FILES-AND-URLS is structured like `tabulated-list-entries'.  After
uploading files, open PARENT-ENTRY."
  (let* ((count 0)
         (progress-reporter
          (make-progress-reporter (format "Uploading %s files: " (length files-and-urls)) 0 (length files-and-urls)))
         (queue (make-plz-queue
                 :limit 2
                 :finally (lambda ()
                            (progress-reporter-done progress-reporter)
                            (hyperdrive-open parent-entry)
                            (with-current-buffer (get-buffer-create "*hyperdrive-mirror*")
                              (setq-local hyperdrive-mirror-already-uploaded t))))))
    (pcase-dolist (`(,_id [,file ,url]) files-and-urls)
      (hyperdrive-upload-file file (hyperdrive-url-entry url)
        :queue queue
        ;; TODO: Error handling (e.g. in case one or more files fails to upload).
        :then (lambda (_)
                (progress-reporter-update progress-reporter (cl-incf count)))))))

(defun hyperdrive-mirror-do-upload ()
  "Upload files in current \"*hyperdrive-mirror*\" buffer."
  (interactive nil hyperdrive-describe-mode)
  (if (and tabulated-list-entries hyperdrive-mirror-parent-entry)
      (when (or (not hyperdrive-mirror-already-uploaded) (yes-or-no-p "Already uploaded files. Upload again?"))
        (hyperdrive--mirror tabulated-list-entries hyperdrive-mirror-parent-entry))
    (user-error "Hyperdrive: Missing information about files to upload. Are you in a \"*hyperdrive-mirror*\" buffer?")))

(defvar-keymap hyperdrive-mirror-mode-map
  :parent  tabulated-list-mode-map
  :doc "Local keymap for `hyperdrive-mirror-mode' buffers."
  "C-c C-c"   #'hyperdrive-mirror-do-upload)

(define-derived-mode hyperdrive-mirror-mode tabulated-list-mode
  "Hyperdrive-mirror"
  "Major mode for buffers for describing hyperdrives."
  :group 'hyperdrive
  :interactive nil
  (setq tabulated-list-format [("From file" 60 t)
                               ("To URL" 60 t)])
  (tabulated-list-init-header))

(defun hyperdrive-read-files ()
  "Return list of files read from the user."
  (cl-loop for file = (read-file-name "File (blank to stop): ")
           while (not (string-blank-p file))
           collect file))

(cl-defun hyperdrive-upload-files (files hyperdrive &key (target-directory "/"))
  "Upload FILES to TARGET-DIRECTORY in HYPERDRIVE.

Prefix argument forces `hyperdrive-complete-hyperdrive' to prompt
for a hyperdrive."
  (interactive
   (let* ((hyperdrive (hyperdrive-complete-hyperdrive :predicate #'hyperdrive-writablep
                                                      :force-prompt current-prefix-arg))
          (files (hyperdrive-read-files))
          ;; TODO: Consider offering target dirs in hyperdrive with completion.
          (target-dir (hyperdrive-read-path :hyperdrive hyperdrive :prompt "Target directory in «%s»" :default "/")))
     (list files hyperdrive :target-directory target-dir)))
  (cl-assert (cl-notany #'file-directory-p files))
  (cl-assert (cl-every #'file-readable-p files))
  (setf files (delete-dups files))
  (dolist (file files)
    (unless (= 1 (cl-count (file-name-nondirectory file) files
                           :test #'equal :key #'file-name-nondirectory))
      (user-error "Can't upload multiple files with same name: %S" (file-name-nondirectory file))))
  (setf target-directory (hyperdrive--format-path target-directory :directoryp t))
  (let ((queue (make-plz-queue
                :limit hyperdrive-queue-size
                :finally (lambda ()
                           ;; FIXME: Offer more informative message in case of errors?
                           (hyperdrive-open (hyperdrive-make-entry :hyperdrive hyperdrive
                                                                   :path target-directory
                                                                   :encode t))
                           (hyperdrive-message "Uploaded %s files." (length files))))))
    (dolist (file files)
      (let* ((path (file-name-concat target-directory (file-name-nondirectory file)))
             (entry (hyperdrive-make-entry :hyperdrive hyperdrive :path path :encode t)))
        ;; TODO: Handle failures? Retry?
        (hyperdrive-upload-file file entry :queue queue :then #'ignore)))
    (plz-run queue)))

;;;; Footer

(provide 'hyperdrive)
;;; hyperdrive.el ends here
