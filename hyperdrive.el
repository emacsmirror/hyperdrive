;;; hyperdrive.el --- P2P filesystem  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joseph Turner <joseph@ushin.org>

;; Author: Joseph Turner
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <joseph@ushin.org>
;; Created: 2022
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (map "3.0") (compat "29.1.4.0") (plz "0.7") (persist "0.5"))
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


;; Hyperdrive is a P2P, real-time, local-first, versioned filesystem
;; designed for easy peer-to-peer file sharing.  hyperdrive.el is an
;; independent project built by https://ushin.org which provides an
;; Emacs interface for managing hyperdrives.


;;;; Installation:

;; hyperdrive.el requires Emacs version 27.1 or later.

;; hyperdrive.el is available on MELPA:
;; https://melpa.org/#/getting-started

;; Once you've set up MELPA, run
;; M-x package-install RET hyperdrive RET

;; hyperdrive.el relies on hyper-gateway for connecting to the P2P network:
;; https://github.com/RangerMauve/hyper-gateway

;; Installation instructions:
;; https://github.com/RangerMauve/hyper-gateway#how-do-i-install-hyper-gateway

;;; Code:

;; TODO: When requiring Emacs 28+, consider using symbol shorthands to
;; reduce how many times we have to type "hyperdrive".

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

(require 'hyperdrive-lib)
(require 'hyperdrive-handlers)
(require 'hyperdrive-org)

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

;;;###autoload
(defun hyperdrive-start ()
  "Start `hyper-gateway' systemd service if not already running."
  (interactive)
  ;; TODO: Verify that the latest version is installed.  See: <https://github.com/RangerMauve/hyper-gateway/issues/9>.
  (let ((buffer (get-buffer-create " *hyperdrive-start*")))
    (unwind-protect
        (unless (zerop (call-process "systemctl" nil (list buffer t) nil "--user" "start" "hyper-gateway.service"))
          (hyperdrive-error "Unable to start hyper-gateway: %S"
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
          (hyperdrive-error "Unable to stop hyper-gateway: %S"
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
    (plz-error (hyperdrive-api-default-else nil (caddr err)))))

;;;###autoload
(defun hyperdrive-new (seed)
  "Open new hyperdrive for SEED.

If SEED is not currently used as the petname for another
hyperdrive, the new hyperdrive's petname will be set to SEED."
  (interactive (list (hyperdrive-read-name :prompt "New hyperdrive seed")))
  (let* ((response (hyperdrive-api 'post (concat "hyper://localhost/?key=" (url-hexify-string seed))))
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

;;;###autoload
(defun hyperdrive-purge (hyperdrive)
  "Purge all data corresponding to HYPERDRIVE."
  (interactive (list (hyperdrive-complete-hyperdrive)))
  (when (yes-or-no-p (format "Purge all data for hyperdrive «%s»? "
                             (hyperdrive--format-hyperdrive hyperdrive)))
    (hyperdrive-purge-no-prompt hyperdrive
      :then (lambda (_response)
              (hyperdrive-message "Purged drive: %s" (hyperdrive--format-hyperdrive hyperdrive)))
      :else (lambda (_plz-error)
              (hyperdrive-error "Unable to purge drive: %s" (hyperdrive--format-hyperdrive hyperdrive))))))

(defun hyperdrive-set-petname (petname hyperdrive)
  "Set HYPERDRIVE's PETNAME.
Entering an empty or blank string unsets PETNAME.
Returns HYPERDRIVE.

Universal prefix argument \\[universal-argument] forces
`hyperdrive-complete-hyperdrive' to prompt for a hyperdrive."
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

Universal prefix argument \\[universal-argument] forces
`hyperdrive-complete-hyperdrive' to prompt for a hyperdrive."
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
            :then (pcase-lambda ((cl-struct plz-response headers))
                    (hyperdrive-message "Unset nickname")
                    (hyperdrive--fill-latest-version hyperdrive headers)
                    (hyperdrive-persist hyperdrive))))
      (setf (alist-get 'name (hyperdrive-metadata hyperdrive)) nickname)
      (hyperdrive-put-metadata hyperdrive
        :then (pcase-lambda ((cl-struct plz-response headers))
                (hyperdrive-message "Set nickname for «%s» to %s"
                                    (hyperdrive--format-hyperdrive hyperdrive)
                                    (hyperdrive--format-host hyperdrive :format '(nickname)))
                (hyperdrive--fill-latest-version hyperdrive headers)
                (hyperdrive-persist hyperdrive))))
    ;; TODO: Consider refreshing buffer names, directory headers, etc, especially host-meta.json entry buffer.
    )
  hyperdrive)

(defun hyperdrive-revert-buffer (&optional _ignore-auto noconfirm)
  "Revert `hyperdrive-mode' buffer by reloading hyperdrive contents.
With NOCONFIRM or when current entry is a directory, revert
without confirmation."
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
  (declare (modes hyperdrive-mode))
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
            (define-key map [remap dired-jump]  #'hyperdrive-up)
            map)
  (if hyperdrive-mode
      (progn
        (setq-local revert-buffer-function #'hyperdrive-revert-buffer
                    bookmark-make-record-function #'hyperdrive-bookmark-make-record)
        (hyperdrive--hack-write-contents-functions))
    (kill-local-variable 'bookmark-make-record-function)
    (kill-local-variable 'revert-buffer-function)
    (setq-local write-contents-functions
                (remove #'hyperdrive--write-contents write-contents-functions))
    (remove-hook 'change-major-mode-hook
                 #'hyperdrive--hack-write-contents-functions 'local)))
;; Making it permanent-local keeps the minor mode active even if the
;; user changes the major mode, so the buffer can still be saved back
;; to the hyperdrive.
(put 'hyperdrive-mode 'permanent-local t)

(defun hyperdrive--hack-write-contents-functions ()
  "Hack `write-contents-functions' for `hyperdrive-mode' in current buffer.
Runs an immediate timer which adds `hyperdrive--write-contents'
to `write-contents-functions'.  Also adds self to
`change-major-mode-hook' so that it will happen again if the
major mode changes (which resets `write-contents-functions' by
calling `kill-all-local-variables').  This is a bit of a hack,
but it seems to be necessary, and to be the cleanest way."
  (run-at-time 0 nil
               (lambda (buffer)
                 (with-current-buffer buffer
                   (cl-pushnew #'hyperdrive--write-contents write-contents-functions)
                   (add-hook 'change-major-mode-hook
                             #'hyperdrive--hack-write-contents-functions nil 'local)))
               (current-buffer)))

;;;###autoload
(defun hyperdrive-find-file (entry)
  "Find hyperdrive ENTRY.
Interactively, prompts for known hyperdrive and path.

With universal prefix argument \\[universal-argument], prompts
for more information.  See `hyperdrive-read-entry' and
`hyperdrive-complete-hyperdrive'."
  (interactive (list (hyperdrive-read-entry :force-prompt current-prefix-arg)))
  (hyperdrive-open entry))

;;;###autoload
(defun hyperdrive-open-url (url)
  "Open hyperdrive URL."
  (interactive (list (hyperdrive-read-url :prompt "Open hyperdrive URL")))
  (hyperdrive-open (hyperdrive-url-entry url)))

(declare-function hyperdrive-history "hyperdrive-history")
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
  ;; FIXME: Some of the synchronous filling functions we've added now cause this to be blocking,
  ;; which is very noticeable when a file can't be loaded from the gateway and eventually times out.
  (let ((hyperdrive (hyperdrive-entry-hyperdrive entry)))
    (hyperdrive-fill entry
      :then (lambda (entry)
              (pcase-let* (((cl-struct hyperdrive-entry type) entry)
                           (handler (alist-get type hyperdrive-type-handlers nil nil #'string-match-p)))
                (unless (hyperdrive--entry-directory-p entry)
                  ;; No need to fill latest version for directories,
                  ;; since we do it in `hyperdrive--fill' already.
                  (hyperdrive-fill-latest-version hyperdrive))
                (hyperdrive-persist hyperdrive)
                (funcall (or handler #'hyperdrive-handler-default) entry :then then)))
      :else (lambda (err)
              (cl-labels ((not-found-action
                            () (if recurse
                                   (hyperdrive-open (hyperdrive-parent entry) :recurse t)
                                 (pcase (prompt)
                                   ('history (hyperdrive-history entry))
                                   ('up (hyperdrive-open (hyperdrive-parent entry)))
                                   ('recurse (hyperdrive-open (hyperdrive-parent entry) :recurse t)))))
                          (prompt
                            () (pcase-exhaustive
                                   (read-answer (format "URL not found: \"%s\". " (hyperdrive-entry-url entry))
                                                '(("history" ?h "open version history")
                                                  ("up" ?u "open parent directory")
                                                  ("recurse" ?r "go up until a directory is found")
                                                  ("exit" ?q "exit")))
                                 ("history" 'history)
                                 ("up" 'up)
                                 ("recurse" 'recurse)
                                 ("exit" nil))))
                (pcase (plz-response-status (plz-error-response err))
                  (404 ;; Path not found.
                   (cond
                    ((equal (hyperdrive-entry-path entry) "/")
                     ;; Root directory not found: Drive has not been
                     ;; loaded locally, and no peers are found seeding it.
                     (hyperdrive-message "No peers found for %s" (hyperdrive-entry-url entry)))
                    ((and (not (hyperdrive--entry-directory-p entry))
                          (hyperdrive-writablep hyperdrive)
                          (not (hyperdrive-entry-version entry)))
                     ;; Entry is a writable file: create a new buffer
                     ;; that will be saved to its path.
                     (if-let ((buffer (get-buffer (hyperdrive--entry-buffer-name entry))))
                         ;; Buffer already exists: likely the user deleted the entry
                         ;; without killing the buffer.  Switch to the buffer and
                         ;; alert the user that the entry no longer exists.
                         (progn
                           (switch-to-buffer buffer)
                           (message "Entry no longer exists!  %s" (hyperdrive-entry-description entry)))
                       ;; Make and switch to new buffer.
                       (switch-to-buffer (hyperdrive--get-buffer-create entry))))
                    (t
                     ;; Hyperdrive entry is not writable: prompt for action.
                     (not-found-action))))
                  (500 ;; Generic error, likely a mistyped URL
                   (hyperdrive-message "Generic hyper-gateway status 500 error. Is this URL correct? %s"
                                       (hyperdrive-entry-url entry)))
                  (_ (hyperdrive-message "Unable to load URL \"%s\": %S"
                                         (hyperdrive-entry-url entry) err))))))))

;;;###autoload
(defun hyperdrive-download-entry (entry filename)
  "Download ENTRY to FILENAME on disk.
Interactively, downloads current hyperdrive file.  If current
buffer is not a hyperdrive file, prompts with
`hyperdrive-read-entry'.

With universal prefix argument \\[universal-argument], prompts
for more information.  See `hyperdrive-read-entry' and
`hyperdrive-complete-hyperdrive'."
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

With universal prefix argument \\[universal-argument], prompts
for more information.  See `hyperdrive-read-entry' and
`hyperdrive-complete-hyperdrive'."
  (interactive (list (hyperdrive-read-entry :predicate #'hyperdrive-writablep
                                            :force-prompt current-prefix-arg
                                            :default-path (when (and hyperdrive-current-entry
                                                                     (not current-prefix-arg))
                                                            (hyperdrive-entry-path hyperdrive-current-entry))
                                            :allow-version-p nil)))
  ;; FIXME: Overwrites without prompting if file exists.
  (ignore overwritep)
  (pcase-let (((cl-struct hyperdrive-entry hyperdrive name) entry)
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
                  (hyperdrive-fill-latest-version hyperdrive)
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
                (hyperdrive-message "Unable to write: %S: %S" name message)
                (with-current-buffer buffer
                  (set-buffer-modified-p t)))))
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
  (declare (modes hyperdrive-mode))
  (interactive (list hyperdrive-current-entry))
  (let ((url (hyperdrive-entry-url entry)))
    (kill-new url)
    (hyperdrive-message "%s" url)))

(defun hyperdrive-up ()
  "Go up to parent directory."
  (declare (modes hyperdrive-mode))
  (interactive)
  (if-let ((parent (hyperdrive-parent hyperdrive-current-entry)))
      (hyperdrive-open parent)
    (hyperdrive-user-error "At root directory")))

(defvar-keymap hyperdrive-up-map
  :doc "Keymap to repeat `hyperdrive-up'.  Used in `repeat-mode'."
  :repeat t
  "j"   #'hyperdrive-up
  "C-j" #'hyperdrive-up)

(defun hyperdrive-previous-version (entry)
  "Show previous version of ENTRY."
  (declare (modes hyperdrive-mode))
  (interactive (list hyperdrive-current-entry))
  (if-let ((previous-entry (hyperdrive-entry-previous entry)))
      (hyperdrive-find-file previous-entry)
    (hyperdrive-message "At earliest known version of entry")))

(defun hyperdrive-next-version (entry &optional no-recurse)
  "Show next version of ENTRY.
When NO-RECURSE is non-nil and the next version range's :EXISTSP
value is unknown, call `hyperdrive-fill-version-ranges' and
recurse, passing NO-RECURSE t to `hyperdrive-next-version'."
  (declare (modes hyperdrive-mode))
  (interactive (list hyperdrive-current-entry))
  ;; TODO: Consider using `hyperdrive-user-error' to reduce nesting.
  (if (hyperdrive-entry-version entry)
      (if (hyperdrive--entry-directory-p entry)
          ;; For directories, increment the version number by one.
          (let ((copy (hyperdrive-copy-tree entry t)))
            (cl-incf (hyperdrive-entry-version copy))
            (hyperdrive-find-file copy))
        (pcase-let ((latest-version (hyperdrive-fill-latest-version (hyperdrive-entry-hyperdrive entry)))
                    (`(,_range-start . ,(map (:range-end range-end))) (hyperdrive-entry-version-range entry)))
          (if (eq latest-version range-end)
              (let ((copy (hyperdrive-copy-tree entry t)))
                ;; NOTE: There is an unlikely race condition here. It's possible that after
                ;; the `hyperdrive-fill-latest-version' call, this entry was updated.
                (setf (hyperdrive-entry-version copy) nil)
                (hyperdrive-find-file copy)
                (hyperdrive-message "Already at latest version of entry."))
            (pcase-let* ((next-range-start (1+ range-end))
                         ((map (:existsp next-range-existsp) (:range-end next-range-end))
                          (map-elt (hyperdrive-entry-version-ranges-no-gaps entry) next-range-start)))
              (pcase next-range-existsp
                ('t
                 ;; Known existent, open it:
                 (let ((copy (hyperdrive-copy-tree entry t)))
                   (if (eq next-range-end latest-version)
                       ;; This is the latest version: remove version number
                       (setf (hyperdrive-entry-version copy) nil)
                     (setf (hyperdrive-entry-version copy) next-range-start))
                   (hyperdrive-find-file copy)))
                ('nil
                 ;; Known nonexistent, warn:
                 (hyperdrive-message "Entry deleted after this version. Try M-x hyperdrive-history"))
                ('unknown
                 ;; Unknown existence, either warn or recurse:
                 (if no-recurse
                     (hyperdrive-message "Next version unknown. Try M-x hyperdrive-history")
                   (hyperdrive-message "Loading history to find next version...")
                   (hyperdrive-fill-version-ranges entry
                     :then (lambda () (hyperdrive-next-version entry t))))))))))
    (hyperdrive-message
     (substitute-command-keys
      (format "Already at latest version of entry; consider reverting buffer with %s to check for newer versions"
              (if (fboundp 'revert-buffer-quick)
                  "\\[revert-buffer-quick]"
                "\\[revert-buffer]"))))))

;;;; Bookmark support

(require 'bookmark)

(defun hyperdrive-bookmark-make-record ()
  "Return a bookmark record for current hyperdrive buffer.
Works in `hyperdrive-mode' and `hyperdrive-dir-mode' buffers."
  (let ((bookmark (bookmark-make-record-default 'no-file)))
    (setf (alist-get 'handler bookmark) #'hyperdrive-bookmark-handler
          (alist-get 'location bookmark) (hyperdrive-entry-url hyperdrive-current-entry))
    (cons (format "hyperdrive: %s" (hyperdrive-entry-description hyperdrive-current-entry)) bookmark)))

;;;###autoload
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

;;;###autoload
(cl-defun hyperdrive-upload-file
    (filename entry &key queue
              (then (lambda (&rest _ignore)
                      (hyperdrive-open (hyperdrive-parent entry))
                      (hyperdrive-message "Uploaded: \"%s\"." (hyperdrive-entry-url entry)))))
  "Upload FILENAME to ENTRY.
Interactively, read FILENAME and ENTRY from the user.
After successful upload, call THEN.  When QUEUE, use it.

With universal prefix argument \\[universal-argument], prompts
for more information.  See `hyperdrive-read-entry' and
`hyperdrive-complete-hyperdrive'."
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

(defun hyperdrive-read-files ()
  "Return list of files read from the user."
  (cl-loop for file = (read-file-name "File (blank to stop): ")
           while (not (string-blank-p file))
           collect file))

(cl-defun hyperdrive-upload-files (files hyperdrive &key (target-directory "/"))
  "Upload FILES to TARGET-DIRECTORY in HYPERDRIVE.

Universal prefix argument \\[universal-argument] forces
`hyperdrive-complete-hyperdrive' to prompt for a hyperdrive."
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
      (hyperdrive-user-error "Can't upload multiple files with same name: %S" (file-name-nondirectory file))))
  (setf target-directory (hyperdrive--format-path target-directory :directoryp t))
  (let ((queue (make-plz-queue
                :limit hyperdrive-queue-size
                :finally (lambda ()
                           ;; FIXME: Offer more informative message in case of errors?
                           (hyperdrive-open (hyperdrive-entry-create :hyperdrive hyperdrive
                                                                     :path target-directory
                                                                     :encode t))
                           (hyperdrive-message "Uploaded %s files." (length files))))))
    (dolist (file files)
      (let* ((path (file-name-concat target-directory (file-name-nondirectory file)))
             (entry (hyperdrive-entry-create :hyperdrive hyperdrive :path path :encode t)))
        ;; TODO: Handle failures? Retry?
        (hyperdrive-upload-file file entry :queue queue :then #'ignore)))
    (plz-run queue)))

;;;; Footer

(provide 'hyperdrive)
;;; hyperdrive.el ends here
