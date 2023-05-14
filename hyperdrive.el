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

(defcustom hyperdrive-download-directory (expand-file-name "~/")
  "Location where `hyperdrive-download-url' will download files."
  ;; TODO: Default to `eww-download-directory'
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
;; TODO: File a bug report against persist.el.
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

(defun hyperdrive-describe-hyperdrive (hyperdrive)
  "Display various information about HYPERDRIVE."
  ;; TODO: Add hyperdrive-describe-hyperdrive mode with revert and bury buffer functions
  (interactive (list (hyperdrive-complete-hyperdrive)))
  (with-current-buffer (get-buffer-create
                        (format "*Hyperdrive: %s*"
                                (hyperdrive--format-host hyperdrive :format '(short-key)
                                                         :with-label t)))
    (special-mode)
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

(defun hyperdrive-status ()
  "Say whether `hyper-gateway' systemd service is running."
  (interactive)
  (if (hyperdrive--gateway-active-p)
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

;; TODO: Command to upload one or more files.

(defun hyperdrive-revert-buffer (&optional _ignore-auto noconfirm)
  "Revert `hyperdrive-mode' buffer by reloading hyperdrive contents."
  (when (or noconfirm
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
  (interactive)
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
            ;; TODO: [#C] Redo this command.
            ;; (define-key map [remap dired-jump]  #'hyperdrive-up-directory)
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
Interactively, prompts for known hyperdrive and path."
  (interactive (list (hyperdrive-read-entry)))
  (hyperdrive-open entry))

(defun hyperdrive-open-url (url)
  "Open hyperdrive URL."
  (interactive (list (read-string "Hyperdrive URL: ")))
  (hyperdrive-open (hyperdrive-url-entry url)))

;;;###autoload
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
                                                 ("recurse" ?! "go up until directory found")))
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
                        (cond ((string-suffix-p "/" (hyperdrive-entry-path entry))
                               ;; Path ends in a slash (and hyperdrive does not
                               ;; support empty directories): offer to go up the tree.
                               (go-up))
                              ;; Path does not end in a slash.
                              ((hyperdrive-writablep hyperdrive)
                               ;; Hyperdrive is writable: create a new buffer that will be saved to that path.
                               (switch-to-buffer (hyperdrive--get-buffer-create entry)))
                              (t
                               ;; Hyperdrive not writable: offer to go up.
                               (go-up))))
                       (_ (hyperdrive-message "Unable to load URL \"%s\": %S"
                                              (hyperdrive-entry-url entry) plz-error)))))))))))

;;;###autoload
(defun hyperdrive-download-entry (entry filename)
  "Download ENTRY to FILENAME on disk.
Interactively, downloads current hyperdrive file.  If current
buffer is not a hyperdrive file, prompts with
`hyperdrive-read-entry'."
  (interactive
   (pcase-let* ((entry (if hyperdrive-mode
                           hyperdrive-current-entry
                         (hyperdrive-read-entry)))
                ((cl-struct hyperdrive-entry name) entry)
                (read-filename (read-string "Filename: " (expand-file-name name hyperdrive-download-directory))))
     (list entry read-filename)))
  (hyperdrive-download-url (hyperdrive-entry-url entry) filename))

;;;###autoload
(defun hyperdrive-download-url (url filename)
  "Load contents at URL as a file to store on disk at FILENAME."
  (interactive
   (let* ((read-url (read-string "Hyperdrive URL: "))
          (name (hyperdrive-entry-name (hyperdrive-url-entry read-url)))
          (read-filename (read-string "Filename: " (expand-file-name name hyperdrive-download-directory))))
     (list read-url read-filename)))
  (hyperdrive-api 'get url :as `(file ,filename)))

;;;###autoload
(defun hyperdrive-save-buffer (entry)
  "Save ENTRY to hyperdrive (interactively, the current buffer).
If buffer was not hyperdrive-backed, it becomes so."
  ;; TODO: Improve docstrings of `hyperdrive-save-buffer' and
  ;; `hyperdrive-write-buffer' after we've sorted out their behavior.
  (interactive
   (list (if hyperdrive-mode
             hyperdrive-current-entry
           (hyperdrive-read-entry :predicate #'hyperdrive-writablep))))
  (hyperdrive-write-buffer entry 'overwrite))

;;;###autoload
(defun hyperdrive-write-buffer (entry &optional overwritep)
  "Write current buffer to new hyperdrive ENTRY.
If file already exists and OVERWRITEP is nil, prompt the user to
overwrite."
  (interactive (list (hyperdrive-read-entry :predicate #'hyperdrive-writablep)))
  ;; FIXME: Overwrites without prompting if file exists.  Make new
  ;; --writable-p based on
  ;; <https://github.com/RangerMauve/hypercore-fetch/issues/60>.
  (ignore overwritep)
  (unless hyperdrive-mode
    ;; TODO: Remove faces/overlays that might be applied to current buffer.
    ;; I can confirm that overlays are not removed when
    ;; `hyperdrive-write-buffer' is called from a magit log buffer.
    (hyperdrive-mode))
  (setq-local hyperdrive-current-entry entry)
  (pcase-let (((cl-struct hyperdrive-entry name) entry)
              (url (hyperdrive-entry-url entry))
              (buffer (current-buffer)))
    (hyperdrive-write entry
      :body (save-restriction
              (widen)
              (buffer-substring-no-properties (point-min) (point-max)))
      :then (lambda (_response)
              ;; TODO: Fill entry after writing it so that
              ;; hyperdrive-previous-version works correctly after
              ;; writing a hyperdrive file.
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
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
  "Call `hyperdrive-save-buffer' for the current buffer.
To be used in `write-contents-functions'."
  (cl-assert hyperdrive-mode)
  (hyperdrive-save-buffer hyperdrive-current-entry))

(defun hyperdrive-copy-url (entry)
  "Save hyperdrive ENTRY's URL to the kill ring.
Interactively, uses `hyperdrive-current-entry', from either a
hyperdrive directory listing or a `hyperdrive-mode' file buffer."
  (interactive (list hyperdrive-current-entry))
  (let ((url (hyperdrive-entry-url entry)))
    (kill-new url)
    (hyperdrive-message "%s" url)))

(defun hyperdrive-previous-version (entry)
  "Show previous version of ENTRY."
  (interactive (list hyperdrive-current-entry))
  ;; TODO: Nicely handle when called without an entry.
  (if-let ((previous-entry (hyperdrive-entry-previous entry)))
      (hyperdrive-find-file previous-entry)
    (hyperdrive-message "At earliest version of entry")))

;;;; Bookmark support

(require 'bookmark)

(defun hyperdrive-bookmark-make-record ()
  "Return a bookmark record for current hyperdrive buffer.
Works in `hyperdrive-mode' and `hyperdrive-dir-mode' buffers."
  (pcase-let* (((cl-struct hyperdrive-entry path hyperdrive) hyperdrive-current-entry)
               ((cl-struct hyperdrive public-key) hyperdrive)
               (hyperdrive-name (hyperdrive--format-host hyperdrive :format '(petname nickname domain)
                                                         :with-label t))
               ;; We use the default function to make a record, then add our fields to it.
               (bookmark (bookmark-make-record-default 'no-file)))
    ;; Add our fields.
    (cl-loop for (key . value) in
             `((handler . hyperdrive-bookmark-handler)
               (location . ,(hyperdrive-entry-url hyperdrive-current-entry))
               ;; TODO: Support versioned hyperdrive bookmarks.
               ;; TODO: Why do we store -entry-path and -public-key
               ;; here? Location URL alone should be sufficient, and
               ;; we can destructure URL in -bookmark-handler
               (hyperdrive-entry-path . ,path)
               (hyperdrive-public-key . ,public-key))
             do (setf (alist-get key bookmark) value))
    (cons (format "Hyperdrive: %s%s" hyperdrive-name path) bookmark)))

(defun hyperdrive-bookmark-handler (bookmark)
  "Handler for Hyperdrive BOOKMARK."
  (pcase-let* ((`(,_ . ,(map ('hyperdrive-entry-path path) ('hyperdrive-public-key public-key)))
                bookmark)
               (hyperdrive (make-hyperdrive :public-key public-key))
               (entry (hyperdrive-make-entry :hyperdrive hyperdrive :path path)))
    (cond ((null path) nil)
          (t (hyperdrive-open entry
               :then (lambda ()
                       ;; TODO: Once plz.el adds a finalizer callback, ensure that point lands at the correct spot
                       (bookmark-default-handler
                        ;; Don't mutate the original record.
                        (append bookmark `((buffer . ,(current-buffer)))))))))))

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
    (filename entry &key queue (then (lambda (&rest _ignore)
                                       (hyperdrive-open (hyperdrive-parent entry))
                                       (hyperdrive-message "Uploaded: \"%s\"." (hyperdrive-entry-url entry)))))
  "Upload FILENAME to ENTRY.
Interactively, read FILENAME and ENTRY from the user.  When
QUEUE, use it."
  (interactive (let ((filename (read-file-name "Upload file: ")))
                 (list filename
                       (hyperdrive-read-entry :predicate #'hyperdrive-writablep
                                              :name (file-name-nondirectory filename)))))
  (let ((url (hyperdrive-entry-url entry)))
    (hyperdrive-api 'put url :queue queue
      :body `(file ,filename)
      :then then)
    (hyperdrive-message "Uploading to \"%s\"..." url)))

;; TODO: Don't overwrite a hyperdrive file with the same
;; contents. Should we keep a cache of uploaded files and mtimes?

(cl-defun hyperdrive-upload-files (files hyperdrive &key (target-directory "/"))
  "Upload FILES to TARGET-DIRECTORY in HYPERDRIVE."
  (interactive
   (let ((hyperdrive (hyperdrive-complete-hyperdrive :predicate #'hyperdrive-writablep))
         (files (cl-loop for file = (read-file-name "File (blank to stop): ")
                         while (not (string-blank-p file))
                         collect file))
         ;; TODO: Consider offering target dirs in hyperdrive with completion.
         (target-dir (pcase (read-string "Target directory: " nil nil "/")
                       ((pred string-blank-p) "/")
                       (else else))))
     (list files hyperdrive :target-directory target-dir)))
  (cl-assert (cl-notany #'file-directory-p files))
  (cl-assert (cl-every #'file-readable-p files))
  (setf files (delete-dups files))
  (dolist (file files)
    (unless (= 1 (cl-count (file-name-nondirectory file) files
                           :test #'equal :key #'file-name-nondirectory))
      (user-error "Can't upload multiple files with same name: %S" (file-name-nondirectory file))))
  (setf target-directory (file-name-as-directory (expand-file-name target-directory "/")))
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

;; TODO: Calling hyperdrive-upload-files is awkward. Create a wrapper that
;; accepts a public-key/seed instead of a hyperdrive?

;; (defun my/hyperdrive-upload-files-foo ()
;;   "Upload all files inside of \"~/public/\" to hyperdrive with seed \"foo\"."
;;   (interactive)
;;   (let ((files (directory-files-recursively "~/public/" "")))
;;     (hyperdrive-upload-files
;;      (make-hyperdrive :public-key (substring (hyperdrive-seed-url "foo") (length "hyper://")))
;;      files "~/public/")))

;;;; Footer

(provide 'hyperdrive)
;;; hyperdrive.el ends here
