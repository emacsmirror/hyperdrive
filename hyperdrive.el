;;; hyperdrive.el --- P2P filesystem  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023, 2024 USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <~ushin/ushin@lists.sr.ht>
;; Created: 2022
;; Version: 0.4-pre
;; Package-Requires: ((emacs "28.1") (map "3.0") (compat "29.1.4.4") (plz "0.7.2") (persist "0.6") (taxy-magit-section "0.13") (transient "0.6.0"))
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
;; independent project built by https://ushin.org, which provides an
;; Emacs interface for managing hyperdrives.

;;;; Installation:

;; hyperdrive.el requires Emacs version 28.1 or later.

;; hyperdrive.el is available on MELPA:
;; https://melpa.org/#/getting-started

;; Once you've set up MELPA, run
;; M-x package-install RET hyperdrive RET

;; hyperdrive.el relies on hyper-gateway-ushin for connecting to the P2P network:
;; https://git.sr.ht/~ushin/hyper-gateway-ushin

;; Installation instructions:
;; https://git.sr.ht/~ushin/hyper-gateway-ushin/#installation

;;; Code:

;;;; Requirements

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
(require 'hyperdrive-org)

;;;; Links

(defvar browse-url-handlers)
(defvar thing-at-point-uri-schemes)

(defun h/browse-url (url &rest _ignore)
  "Browse hyperdrive URL."
  (h/open-url url))

(require 'browse-url)
(require 'thingatpt)

(cl-pushnew (cons (rx bos "hyper://") #'h/browse-url)
            browse-url-handlers :test #'equal)
(cl-pushnew "hyper://" thing-at-point-uri-schemes :test #'equal)

;;;; Commands

;; TODO[#A]: Command to rename paths.

;;;###autoload
(defun hyperdrive-start ()
  "Start `hyper-gateway-ushin' if not already running.
Calls function set in option `hyperdrive-gateway-start-function',
which see."
  (interactive)
  (cond ((and (h//gateway-ready-p) (h/gateway-live-p))
         (h/message "Gateway already running."))
        ((h//gateway-ready-p)
         (h/message "Gateway already running outside of Emacs."))
        ((h/gateway-live-p)
         (h/message "Gateway already starting."))
        (t (funcall h/gateway-start-function)))
  (h//gateway-wait-for-ready))

;;;###autoload
(defun hyperdrive-stop ()
  "Stop `hyper-gateway-ushin' if running.
Calls function set in option `hyperdrive-gateway-stop-function',
which see."
  (interactive)
  (funcall h/gateway-stop-function))

;;;###autoload
(defun hyperdrive-gateway-version ()
  "Say version number of gateway.
Return version if gateway is running; otherwise signal an error."
  (interactive)
  (h/message "%S" (h//gateway-version)))

;;;###autoload
(defun hyperdrive-new (seed)
  "Open new hyperdrive for SEED.

If SEED is not currently used as the petname for another
hyperdrive, the new hyperdrive's petname will be set to SEED."
  (interactive (list (h/read-name :prompt "New hyperdrive seed")))
  (let* ((response (h/api 'post (concat "hyper://localhost/?key=" (url-hexify-string seed))))
         (url (progn
                ;; NOTE: Working around issue in plz whereby the
                ;; stderr process sentinel sometimes leaves "stderr
                ;; finished" garbage in the response body in older
                ;; Emacs versions.  See: <https://github.com/alphapapa/plz.el/issues/23>.
                (string-match (rx bos (group "hyper://" (1+ nonl))) response)
                (match-string 1 response)))
         (hyperdrive (he/hyperdrive (h/url-entry url))))
    (setf (h/seed hyperdrive) seed)
    (setf (h/writablep hyperdrive) t)
    (unwind-protect
        (h/set-petname seed hyperdrive)
      (h/persist hyperdrive)
      (h/open (h/url-entry url)))))

;;;###autoload
(defun hyperdrive-purge (hyperdrive)
  "Purge all data corresponding to HYPERDRIVE."
  (interactive (list (h/complete-hyperdrive :force-prompt t)))
  (when (yes-or-no-p (format-message "Delete local copy of hyperdrive (data will likely not be recoverable—see manual): `%s'? "
                                     (h//format-hyperdrive hyperdrive)))
    (h/purge-no-prompt hyperdrive
      :then (lambda (_response)
              (h/message "Purged drive: %s" (h//format-hyperdrive hyperdrive)))
      :else (lambda (plz-error)
              (h/error "Unable to purge drive: %s %S" (h//format-hyperdrive hyperdrive) plz-error)))))

;;;###autoload
(defun hyperdrive-set-petname (petname hyperdrive)
  "Set HYPERDRIVE's PETNAME.
Entering an empty or blank string unsets PETNAME.
Returns HYPERDRIVE.

Universal prefix argument \\[universal-argument] forces
`hyperdrive-complete-hyperdrive' to prompt for a hyperdrive."
  (interactive
   (let* ((hyperdrive (h/complete-hyperdrive :force-prompt current-prefix-arg))
          (petname (h/read-name
                    :prompt (format "Petname for `%s' (leave blank to unset)"
                                    (h//format-hyperdrive hyperdrive))
                    :initial-input (h/petname hyperdrive))))
     (list petname hyperdrive)))
  (while-let (((not (equal petname (h/petname hyperdrive))))
              (other-hyperdrive (cl-find petname (hash-table-values h/hyperdrives)
                                         :key #'h/petname :test #'equal)))
    (setf petname (h/read-name
                   :prompt (format "%S already assigned as petname to hyperdrive `%s'.  Enter new petname"
                                   petname (h//format-hyperdrive other-hyperdrive))
                   :initial-input (h/petname hyperdrive))))
  (if (string-blank-p petname)
      (when (yes-or-no-p (format-message "Unset petname for `%s'? "
                                         (h//format-hyperdrive hyperdrive)))
        (setf (h/petname hyperdrive) nil))
    (setf (h/petname hyperdrive) petname))
  (h/persist hyperdrive)
  ;; TODO: Consider refreshing buffer names, directory headers, etc.
  hyperdrive)

;;;###autoload
(cl-defun hyperdrive-set-nickname (nickname hyperdrive &key (then #'ignore))
  "Set HYPERDRIVE's NICKNAME.
Returns HYPERDRIVE.

Asynchronous callback calls THEN with the updated hyperdrive as
its only argument.

Universal prefix argument \\[universal-argument] forces
`hyperdrive-complete-hyperdrive' to prompt for a hyperdrive."
  (interactive
   (let* ((hyperdrive (h/complete-hyperdrive :predicate #'h/writablep
                                             :force-prompt current-prefix-arg))
          (nickname
           ;; NOTE: Fill metadata first in case the JSON file has been updated manually
           (progn
             (h/fill-metadata hyperdrive)
             (h/read-name
              :prompt (format-message "Nickname for `%s'"
                                      (h//format-hyperdrive hyperdrive))
              :initial-input (alist-get 'name (h/metadata hyperdrive))))))
     (list nickname hyperdrive)))
  (unless (equal nickname (alist-get 'name (h/metadata hyperdrive)))
    (if (string-blank-p nickname)
        (progn
          (cl-callf map-delete (h/metadata hyperdrive) 'name)
          (h/put-metadata hyperdrive
            :then (pcase-lambda ((cl-struct plz-response headers))
                    (h//fill-latest-version hyperdrive headers)
                    (h/persist hyperdrive)
                    (funcall then hyperdrive))))
      (setf (alist-get 'name (h/metadata hyperdrive)) nickname)
      (h/put-metadata hyperdrive
        :then (pcase-lambda ((cl-struct plz-response headers))
                (h//fill-latest-version hyperdrive headers)
                (h/persist hyperdrive)
                (funcall then hyperdrive))))
    ;; TODO: Consider refreshing buffer names, directory headers, etc, especially host-meta.json entry buffer.
    )
  hyperdrive)

(defun h/revert-buffer (&optional _ignore-auto noconfirm)
  "Revert `hyperdrive-mode' buffer by reloading hyperdrive contents.
With NOCONFIRM or when current entry is a directory, revert
without confirmation."
  (when (or (h//entry-directory-p h/current-entry)
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
                     (he/url h/current-entry))))
    ;; TODO: Support before-revert-hook, after-revert-hook, revert-buffer-internal-hook
    ;; Setting the modified flag to nil prevents `h/open'
    ;; from erroring if it has been modified.
    (set-buffer-modified-p nil)
    (h/open h/current-entry)
    t))

(defun h/revert-buffer-quick ()
  "Like `revert-buffer-quick', but works with `hyperdrive-mode' files."
  (interactive nil h/mode)
  (h/revert-buffer nil (not (buffer-modified-p))))

;;;; h/mode

(defvar-local h/mode--state nil
  "Previous state of buffer before `hyperdrive-mode' was activated.
Intended to be passed to `buffer-local-restore-state'.")

;;;###autoload
(define-minor-mode hyperdrive-mode
  ;; TODO: Consider moving h/mode definition to
  ;; hyperdrive-lib.el.  (Since it's used in multiple files.)
  "Minor mode for buffers opened from hyperdrives."
  :global nil
  :interactive nil
  :group 'hyperdrive
  :lighter " hyperdrive"
  :keymap '(([remap revert-buffer-quick] . h/revert-buffer-quick)
            ([remap dired-jump] .  h/up))
  (if h/mode
      (progn
        (setq-local h/mode--state
                    (buffer-local-set-state
                     revert-buffer-function #'h/revert-buffer
                     bookmark-make-record-function #'h/bookmark-make-record
                     write-contents-functions (cl-adjoin #'h//write-contents write-contents-functions)
                     ;; TODO: Modify buffer-local value of `save-some-buffers-action-alist'
                     ;; to allow diffing modified buffer with hyperdrive file
                     buffer-offer-save t))
        (add-hook 'after-change-major-mode-hook
                  #'h//hack-write-contents-functions nil 'local)
        ;; TODO: Consider checking for existing advice before adding our own.
        (advice-add #'org-insert-link :after #'h/org--insert-link-after-advice))
    (buffer-local-restore-state h/mode--state)
    (remove-hook 'after-change-major-mode-hook
                 #'h//hack-write-contents-functions 'local)
    ;; FIXME: Only remove advice when all h/mode buffers are killed.
    ;; (advice-remove #'org-insert-link #'hyperdrive-org--insert-link)
    ))
;; Making it permanent-local keeps the minor mode active even if the
;; user changes the major mode, so the buffer can still be saved back
;; to the hyperdrive.
(put 'h/mode 'permanent-local t)

(defun h//hack-write-contents-functions ()
  "Hack `write-contents-functions' for `hyperdrive-mode' in current buffer.
Ensures that hyperdrive buffers can still be saved after the
major mode changes (which resets `write-contents-functions' by
calling `kill-all-local-variables')."
  (cl-pushnew #'h//write-contents write-contents-functions))
(put 'h//hack-write-contents-functions 'permanent-local-hook t)

;;;###autoload
(defun hyperdrive-find-file (entry)
  "Find hyperdrive ENTRY.
Interactively, prompt for known hyperdrive and path.
With universal prefix argument \\[universal-argument], prompt for version."
  (interactive (list (h/read-entry :read-version current-prefix-arg)))
  (h/open entry))

;;;###autoload
(defun hyperdrive-view-file (entry)
  "View ENTRY in `view-mode', returning to previous buffer when done.
Interactively, prompt for known hyperdrive and path.
With universal prefix argument \\[universal-argument], prompt for version."
  ;; TODO: Stay in `view-mode' after
  ;; `hyperdrive-previous-version'/`hyperdrive-next-version'. This may
  ;; require another minor mode.
  (interactive (list (h/read-entry :read-version current-prefix-arg)))
  (h/open entry
    ;; `view-buffer' checks the mode-class symbol property of
    ;; `major-mode' and avoids putting directory buffers in `view-mode'.
    :createp nil :then (lambda () (view-buffer (current-buffer)))))

;;;###autoload
(defun hyperdrive-open-url (url)
  "Open hyperdrive URL."
  (interactive (list (h/read-url :prompt "Open hyperdrive URL")))
  (h/open (h/url-entry url)))

;;;###autoload
(cl-defun hyperdrive-delete (entry &key (then #'ignore) (else #'ignore))
  "Delete ENTRY, then call THEN with response.
Call ELSE with `plz-error' struct if request fails.
Interactively, delete current file ENTRY or ENTRY at point in a
directory.  Otherwise, or with universal prefix argument
\\[universal-argument], prompt for ENTRY."
  (declare (indent defun))
  (interactive
   (let* ((entry (h//context-entry :latest-version t))
          (description (h//format-entry entry))
          (buffer (current-buffer)))
     (when (and (h//entry-directory-p entry)
                (or (eq entry h/current-entry)
                    (string= "../" (alist-get 'display-name (he/etc entry)))))
       (h/user-error "Won't delete from within"))
     (when (and (yes-or-no-p (format-message "Delete `%s'? " description))
                (or (not (h//entry-directory-p entry))
                    (yes-or-no-p (format-message "Recursively delete `%s'? "
                                                 description))))
       (list entry
             :then (lambda (_)
                     (when (and (buffer-live-p buffer)
                                (eq 'h/dir-mode (buffer-local-value 'major-mode buffer)))
                       (with-current-buffer buffer
                         (revert-buffer)))
                     (h/message "Deleted: `%s' (Deleted files can be accessed from prior versions of the hyperdrive.)" description))
             :else (lambda (plz-error)
                     (h/message "Unable to delete `%s': %S" description plz-error))))))
  (h/api 'delete (he/url entry)
    :as 'response
    :then (lambda (response)
            (pcase-let* (((cl-struct plz-response headers) response)
                         ((map etag) headers)
                         (nonexistent-entry (h/copy-tree entry t)))
              (unless (h//entry-directory-p entry)
                ;; FIXME: hypercore-fetch bug doesn't update version
                ;; number when deleting a directory.
                (setf (he/version nonexistent-entry) (string-to-number etag))
                (h//fill-latest-version (he/hyperdrive entry) headers)
                (h/update-nonexistent-version-range nonexistent-entry))
              ;; Since there's no way for `h//write-contents' to run when
              ;; `buffer-modified-p' returns nil, this is a workaround to ensure that
              ;; `save-buffer' re-saves files after they've been deleted.
              (dolist (buf (match-buffers (lambda (buf deleted-entry)
                                            (and-let* ((current-entry (buffer-local-value 'h/current-entry buf)))
                                              (he/equal-p current-entry deleted-entry)))
                                          nil entry))
                (with-current-buffer buf
                  (set-buffer-modified-p t)))
              (funcall then response)))
    :else else))

;;;###autoload
(defun hyperdrive-download (entry filename)
  "Download ENTRY to FILENAME on disk.
Interactively, download current hyperdrive file or file at point
in a directory.  Otherwise, or with universal prefix argument
\\[universal-argument], prompt for ENTRY."
  (interactive
   (pcase-let* ((entry (h//context-entry))
                ((cl-struct hyperdrive-entry name) entry)
                (read-filename (read-file-name "Filename: " (expand-file-name name h/download-directory))))
     (list entry read-filename)))
  (h/download-url (he/url entry) filename))

;;;###autoload
(defun hyperdrive-download-url (url filename)
  "Load contents at URL as a file to store on disk at FILENAME."
  ;; TODO: Handle directory URLs (recursively download contents?)
  (interactive
   (let* ((read-url (h/read-url :prompt "Download hyperdrive URL"))
          (name (he/name (h/url-entry read-url)))
          (read-filename (read-file-name "Filename: " (expand-file-name name h/download-directory))))
     (list read-url read-filename)))
  (when (or (not (file-exists-p filename))
            (yes-or-no-p (format "File %s already exists; overwrite anyway? " (expand-file-name filename))))
    (when (file-exists-p filename)
      ;; plz.el will not overwrite existing files: ensure there's no file there.
      (delete-file filename))
    (h/api 'get url :as `(file ,filename))))

;;;###autoload
(defun hyperdrive-write-buffer (entry &optional overwritep)
  "Write current buffer to new hyperdrive ENTRY.
If file already exists and OVERWRITEP is nil, prompt the user to
overwrite.

With universal prefix argument \\[universal-argument], overwrite
without prompting.

This function is for interactive use only; for non-interactive
use, see `hyperdrive-write'."
  (interactive (list (h/read-entry :predicate #'h/writablep
                                   :default-path (and h/current-entry
                                                      (he/path h/current-entry))
                                   :latest-version t)
                     current-prefix-arg))
  (pcase-let (((cl-struct hyperdrive-entry hyperdrive name) entry)
              (url (he/url entry))
              (buffer (current-buffer))
              (buffer-visiting-entry (h//find-buffer-visiting entry)))
    (unless (or overwritep (not (he/at nil entry)))
      (unless (y-or-n-p
	       (format "File %s exists; overwrite?" (h//format-entry entry)))
        (h/user-error "Canceled"))
      (when (buffer-live-p buffer-visiting-entry)
        (unless (y-or-n-p (format "A buffer is visiting %s; proceed?"
                                  (h//format-entry entry)))
          (h/user-error "Aborted"))))
    (h/write entry
      :body (without-restriction
              (buffer-substring-no-properties (point-min) (point-max)))
      :then (lambda (response)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (unless h/mode
                    (h//clean-buffer)
                    (when (member (hyperdrive-public-key hyperdrive)
                                  h/safe-hyperdrives)
                      (let ((buffer-file-name (he/name entry)))
                        (set-auto-mode)))
                    (h/mode))
                  ;; NOTE: `h/fill-latest-version' must come before
                  ;; `h//fill' because the latter calls
                  ;; `h/update-existent-version-range' internally.
                  (h/fill-latest-version hyperdrive)
                  (h//fill entry (plz-response-headers response))
                  ;; PUT responses only include ETag and Last-Modified
                  ;; headers, so we need to set other entry metadata manually.
                  ;; FIXME: For large buffers, `buffer-size' returns a different
                  ;; value than hyper-gateway-ushin's Content-Length header.
                  (setf (he/size entry) (buffer-size))
                  ;; FIXME: Will entry type ever be anything besides text/plain?
                  ;;        /.well-known/host-meta.json ?
                  (setf (he/type entry) "text/plain; charset=utf-8")
                  (setq-local h/current-entry entry)
                  (setf buffer-file-name nil)
                  (unless (eq buffer buffer-visiting-entry)
                    (when (buffer-live-p buffer-visiting-entry)
                      (kill-buffer buffer-visiting-entry))
                    (rename-buffer (h//generate-new-buffer-name entry)))
                  (set-buffer-modified-p nil)
                  ;; Update the visited file modtime so undo commands
                  ;; correctly set the buffer-modified flag.  We just
                  ;; use `current-time' here since it's good enough
                  ;; and lets us avoid making another request for
                  ;; metadata.
                  (set-visited-file-modtime (current-time))))
              (h/message "Wrote: %S to \"%s\"" name url))
      :else (lambda (plz-error)
              (h/message "Unable to write: %S: %S" name plz-error)))
    (h/message "Saving to \"%s\"..." url)
    ;; TODO: Reload relevant hyperdrive-dir buffers after writing buffer (if ewoc buffers display version, then possibly all ewoc buffers for a given hyperdrive should be reloaded)
    ))

(defun h//write-contents ()
  "Call `hyperdrive-write-buffer' for the current buffer.
To be used in `write-contents-functions'."
  (cl-assert h/mode)
  (h/write-buffer h/current-entry t))

(defun h/copy-url (entry)
  "Save hyperdrive ENTRY's URL to the kill ring.
Interactively, uses `hyperdrive-current-entry', from either a
hyperdrive directory listing or a `hyperdrive-mode' file buffer."
  (interactive (list h/current-entry) h/mode)
  (let ((url (he/url entry)))
    (kill-new url)
    (h/message "%s" url)))

(cl-defun h/up (entry &key (then nil then-set-p))
  "Go up to parent directory of ENTRY.
Interactively, use the `hyperdrive-current-entry'.  If THEN, pass
it to `hyperdrive-open'."
  (interactive (progn
                 (unless (and h/mode h/current-entry)
                   (user-error "Not a hyperdrive buffer"))
                 (list h/current-entry))
               h/mode)
  (if-let ((parent (h/parent entry)))
      ;; TODO: Go to entry in parent directory.
      (if then-set-p
          (h/open parent :then then)
        ;; Allow default callback to be used.
        (h/open parent))
    (h/user-error "At root directory")))

(defvar-keymap h/up-map
  :doc "Keymap to repeat `hyperdrive-up'.  Used in `repeat-mode'."
  :repeat t
  "j"   #'h/up
  "C-j" #'h/up)

;;;###autoload
(define-minor-mode hyperdrive-blob-mode
  "Minor mode for visiting previous versions of hyperdrive files."
  :global nil
  :interactive nil
  :group 'hyperdrive
  :lighter " hyperdrive-blob"
  :keymap '(("n" . h/open-next-version)
            ("p" . h/open-previous-version)
            ("q" . kill-current-buffer)))

(defun h/open-previous-version (entry)
  "Open previous version of ENTRY."
  (interactive (list h/current-entry) h/mode)
  (if-let ((previous-entry (he/previous entry)))
      (h/open previous-entry)
    (h/message "%s does not exist at version %s. Try \\[hyperdrive-history]"
               (h//format-entry entry "[%H] %p")
               (1- (car (he/version-range entry))))))

(defun h/open-next-version (entry)
  "Open next version of ENTRY."
  (interactive (list h/current-entry) h/mode)
  (pcase-exhaustive (he/next entry)
    ((and (pred (eq entry)) next-entry)
     ;; ENTRY already at latest version: open and say `revert-buffer'.
     (h/open next-entry)
     (h/message
      "Already at latest version of entry; consider reverting buffer with %s to check for newer versions"
      (substitute-command-keys
       (if (fboundp 'revert-buffer-quick)
           "\\[revert-buffer-quick]"
         "\\[revert-buffer]"))))
    ('nil ;; Known nonexistent: suggest `h/history'.
     (h/message "Entry deleted after this version. Try \\[hyperdrive-history]"))
    ('unknown ;; Unknown existence: suggest `h/history'.
     (h/message "Next version unknown. Try \\[hyperdrive-history]"))
    ((and (pred he/p) next-entry)
     (h/open next-entry))))

(defun h/open-at-version (entry version)
  "Open ENTRY at VERSION.
Nil VERSION means open the entry at its hyperdrive's latest version."
  (interactive (let ((entry h/current-entry))
                 (list entry (h/read-version
                              :hyperdrive (he/hyperdrive entry)
                              :prompt (format-message "Open `%s' at version (leave blank for latest version)"
                                                      (h//format-entry entry)))))
               h/mode)
  (if-let ((latest-entry (he/at version entry)))
      (h/open latest-entry)
    (h/message "%s does not exist at version %s. Try \\[hyperdrive-history]"
               (h//format-entry
                entry h/default-entry-format-without-version)
               version)))

;;;; Bookmark support

;; TODO: Display entry description instead of full URL in bookmark list view.
(require 'bookmark)

(defun h/bookmark-make-record ()
  "Return a bookmark record for current hyperdrive buffer.
Works in `hyperdrive-mode' and `hyperdrive-dir-mode' buffers."
  (let ((bookmark (bookmark-make-record-default 'no-file)))
    (setf (alist-get 'handler bookmark) #'h/bookmark-handler)
    (setf (alist-get 'location bookmark) (he/url h/current-entry))
    (cons (format "hyperdrive: %s" (h//format-entry h/current-entry)) bookmark)))

;;;###autoload
(defun hyperdrive-bookmark-handler (bookmark)
  "Handler for Hyperdrive BOOKMARK."
  (h/open (h/url-entry (alist-get 'location (cdr bookmark)))
    :then (lambda ()
            (bookmark-default-handler
             ;; We add the buffer property, because we don't want to
             ;; store that in the bookmark record, because the buffer
             ;; name could change in the future, and that would make
             ;; the record invalid, which would cause
             ;; `bookmark-default-handler' to signal an error.
             (append bookmark `((buffer . ,(current-buffer)))))
            (pop-to-buffer (current-buffer) '(display-buffer-same-window)))))
(put 'h/bookmark-handler 'bookmark-handler-type "hyperdrive")

(defun h/bookmark-jump (bookmark)
  "Jump to a Hyperdrive BOOKMARK."
  (interactive
   (progn
     (bookmark-maybe-load-default-file) ; paranoia
     (list
      (completing-read "Open Hyperdrive bookmark: " bookmark-alist
                       (pcase-lambda (`(,_name . ,(map handler)))
                         (equal handler #'h/bookmark-handler))
                       t nil 'bookmark-history))))
  (bookmark-jump bookmark))

(defun h/bookmark-list ()
  "List Hyperdrive bookmarks."
  (interactive)
  (let ((bookmark-alist
         (cl-remove-if-not (pcase-lambda (`(,_name . ,(map handler)))
                             (equal handler #'h/bookmark-handler))
                           bookmark-alist)))
    (call-interactively #'bookmark-bmenu-list)))

;;;; Upload files from disk

;;;###autoload
(cl-defun hyperdrive-upload-file
    (filename entry &key queue
              (then (lambda (&rest _ignore)
                      (h/open (h/parent entry))
                      (h/message "Uploaded: \"%s\"." (he/url entry)))))
  "Upload FILENAME to ENTRY.
Interactively, read FILENAME and ENTRY from the user.
After successful upload, call THEN.  When QUEUE, use it."
  (declare (indent defun))
  (interactive (let ((filename (read-file-name "Upload file: ")))
                 (list filename
                       (h/read-entry :predicate #'h/writablep
                                     :default-path (file-name-nondirectory filename)
                                     :latest-version t))))
  (let ((url (he/url entry))
        (last-modified (let ((system-time-locale "C"))
                         (format-time-string "%Y-%m-%dT%T.%3NZ"
                                             ;; "%a, %-d %b %Y %T %Z"
                                             (file-attribute-modification-time
                                              (file-attributes filename)) t))))
    (h/api 'put url :queue queue
      :body `(file ,filename)
      :headers `(("Last-Modified" . ,last-modified))
      :then then)
    (unless queue
      (h/message "Uploading to \"%s\"..." url))))

(defun h/read-files ()
  "Return list of files read from the user."
  (cl-loop for file = (read-file-name "File (blank to stop): ")
           while (not (string-blank-p file))
           collect file))

(defun h/upload-files (files hyperdrive target-dir)
  "Upload FILES to TARGET-DIR in HYPERDRIVE.

Universal prefix argument \\[universal-argument] forces
`hyperdrive-complete-hyperdrive' to prompt for a hyperdrive."
  (interactive
   (let* ((files (h/read-files))
          (hyperdrive (h/complete-hyperdrive :predicate #'h/writablep
                                             :force-prompt current-prefix-arg))
          ;; TODO: Consider offering target dirs in hyperdrive with completion.
          (target-dir (h/read-path :hyperdrive hyperdrive :prompt "Target directory in `%s'" :default "/")))
     (list files hyperdrive target-dir)))
  (cl-assert (cl-notany #'file-directory-p files))
  (cl-assert (cl-every #'file-readable-p files))
  (setf files (delete-dups files))
  (dolist (file files)
    (unless (= 1 (cl-count (file-name-nondirectory file) files
                           :test #'equal :key #'file-name-nondirectory))
      (h/user-error "Can't upload multiple files with same name: %S" (file-name-nondirectory file))))
  (setf target-dir (h//format-path target-dir :directoryp t))
  (let ((queue (make-plz-queue
                :limit h/queue-limit
                :finally (lambda ()
                           ;; FIXME: Offer more informative message in case of errors?
                           (h/open (he/create :hyperdrive hyperdrive
                                              :path target-dir))
                           (h/message "Uploaded %s files." (length files))))))
    (dolist (file files)
      (let* ((path (file-name-concat target-dir (file-name-nondirectory file)))
             (entry (he/create :hyperdrive hyperdrive :path path)))
        ;; TODO: Handle failures? Retry?
        (h/upload-file file entry :queue queue :then #'ignore)))
    (plz-run queue)))

;;;; Info lookup

(declare-function info-lookup-maybe-add-help "info-look")

(with-eval-after-load 'info-look
  (info-lookup-maybe-add-help
   :mode '(emacs-lisp-mode . "hyperdrive")
   :regexp (rx word-boundary "hyperdrive-" (1+ (not (any "	\n \"'(),[]`‘’"))))
   :doc-spec '((;; INFO-NODE
	        "(hyperdrive)Function Index"
	        ;; TRANS-FUNC
	        nil
	        ;; PREFIX
	        "^ -+ .*: "
	        ;; SUFFIX
	        "\\( \\|$\\)")
	       (;; INFO-NODE
	        "(hyperdrive)Variable Index"
	        ;; TRANS-FUNC
	        nil
	        ;; PREFIX
	        "^ -+ .*: "
	        ;; SUFFIX
	        "\\( \\|$\\)")
               (;; INFO-NODE
	        "(hyperdrive)Keystroke Index"
	        ;; TRANS-FUNC
	        nil
	        ;; PREFIX
	        "^ -+ .*: "
	        ;; SUFFIX
	        "\\( \\|$\\)"))))

;;;; Configure Emacs and EWW for hyper:// URLs.

(require 'url)

(defun h/url-loader (parsed-url)
  "Retrieve URL synchronously.
PARSED-URL must be a URL-struct like the output of
`url-generic-parse-url'.

The return value of this function is the retrieval buffer."
  (cl-check-type parsed-url url "Need a pre-parsed URL.")
  (let* ((url (url-recreate-url parsed-url))
         ;; response-buffer will contain the loaded HTML, and will be deleted at the end of `eww-render'.
         (response-buffer (h/api 'get url :as 'buffer)))
    (with-current-buffer response-buffer
      (widen)
      (goto-char (point-min))
      (while (search-forward (string ?\C-m) nil t)
        ;; Strip CRLF from headers so that `eww-parse-headers' works correctly.
        ;; MAYBE: As an alternative, look at buffer coding systems to
        ;; make `eww-parse-headers' work with CRLFs (since according
        ;; to the HTTP 1 spec, headers are supposed to end with CRLF)
        (replace-match ""))
      (current-buffer))))

(puthash "hyper" '(name "hyper" loader h/url-loader
                        ;; Expand relative paths against host
                        expand-file-name url-default-expander)
         url-scheme-registry)

(require 'eww)
(setf eww-use-browse-url
      (if eww-use-browse-url
          (rx-to-string `(or ,eww-use-browse-url (seq bos "hyper://")))
        (rx bos "hyper://")))

;;;; `kill-buffer-query-functions' integration

(defun h//kill-buffer-possibly-save (buffer)
  "Ask whether to kill modified hyperdrive file BUFFER."
  ;; Mostly copied from `kill-buffer--possibly-save'.
  (cl-assert (and h/mode h/current-entry))
  (let ((response
         (cadr (compat-call
                read-multiple-choice
                (format "Hyperdrive file %s modified; kill anyway?"
                        (h//format-entry h/current-entry))
                '((?y "yes" "kill buffer without saving")
                  (?n "no" "exit without doing anything")
                  (?s "save and then kill" "save the buffer and then kill it"))
                nil nil (and (not use-short-answers)
                             (not (and (fboundp 'use-dialog-box-p)
                                       (with-no-warnings
                                         (use-dialog-box-p)))))))))
    (if (equal response "no")
        nil
      (unless (equal response "yes")
        (with-current-buffer buffer
          (save-buffer)))
      t)))

(defun h/kill-buffer-query-function ()
  "Ask before killing an unsaved hyperdrive file buffer."
  (if (and h/mode
           h/current-entry
           (not (h//entry-directory-p h/current-entry))
           (buffer-modified-p))
      (h//kill-buffer-possibly-save (current-buffer))
    t))

(cl-pushnew #'h/kill-buffer-query-function kill-buffer-query-functions)

;;;;; `easy-menu' integration

(defvar h/menu-bar-menu
  '(("Gateway"
     :label
     (format "Gateway (%s)" (if (h//gateway-ready-p) "on" "off"))
     ["Start Gateway" h/start
      :help "Start hyper-gateway-ushin"]
     ["Stop Gateway" h/stop
      :help "Stop hyper-gateway-ushin"]
     ["Gateway version" h/gateway-version
      :help "Say hyper-gateway-ushin version"])
    "---"
    ["Open URL" h/open-url
     :help "Load a hyperdrive URL"]
    ["New Drive" h/new
     :help "Create a new hyperdrive"]
    ("Drives"
     :active (< 0 (hash-table-count h/hyperdrives))
     :label (if (zerop (hash-table-count h/hyperdrives))
                "Drives (empty)"
              "Drives")
     :filter (lambda (_)
               (cl-labels ((list-drives (drives)
                             (cl-loop for drive in drives
                                      for entry = (he/create :hyperdrive drive)
                                      collect (list (h//format drive)
                                                    (vector "Describe"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let ((h/current-entry ,entry))
                                                                 (call-interactively #'h/describe-hyperdrive)))
                                                            :help "Display information about hyperdrive")
                                                    (vector "Find File"
                                                            `(lambda ()
                                                               (interactive)
                                                               (h/open
                                                                 (h/read-entry
                                                                  :hyperdrive ,drive
                                                                  :read-version current-prefix-arg)))
                                                            :help "Find a file in hyperdrive")
                                                    (vector "View File"
                                                            `(lambda ()
                                                               (interactive)
                                                               (h/view-file
                                                                (h/read-entry
                                                                 :hyperdrive ,drive
                                                                 :read-version current-prefix-arg)))
                                                            :help "View a file in hyperdrive")
                                                    "---"
                                                    (vector "Upload File"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let* ((filename (read-file-name "Upload file: "))
                                                                      (entry (h/read-entry :hyperdrive ,drive
                                                                                           :default-path (file-name-nondirectory filename)
                                                                                           :latest-version t)))
                                                                 (h/upload-file filename entry)))
                                                            :active `(h/writablep ,drive)
                                                            :help "Upload a file to hyperdrive")
                                                    (vector "Upload Files"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let* ((files (h/read-files))
                                                                      (target-dir (h/read-path
                                                                                   :hyperdrive ,drive
                                                                                   :prompt "Target directory in `%s'"
                                                                                   :default "/")))
                                                                 (h/upload-files files ,drive target-dir)))
                                                            :active `(h/writablep ,drive)
                                                            :help "Upload files to hyperdrive")
                                                    (vector "Mirror" #'h/mirror
                                                            ;; TODO: `h/mirror''s interactive form will also prompt
                                                            ;; for a drive. After changing `h/mirror' to use
                                                            ;; Transient.el, we should pass in the default drive argument.
                                                            :active `(h/writablep ,drive)
                                                            :help "Mirror a directory to hyperdrive")
                                                    "---"
                                                    (vector "Petname"
                                                            ;; HACK: We have to unquote the value of the entry because it seems that the filter
                                                            ;; function is called in an environment that doesn't use lexical-binding...?
                                                            ;; TODO: Ask about this and/or file a bug report.
                                                            `(lambda ()
                                                               (interactive)
                                                               (let ((h/current-entry ,entry))
                                                                 (call-interactively #'h/set-petname)))
                                                            :help "Set petname for hyperdrive"
                                                            :label
                                                            (format-message "Set petname: `%s'"
                                                                            (pcase (h/petname drive)
                                                                              (`nil "none")
                                                                              (it it))))
                                                    (vector "Nickname"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let ((h/current-entry ,entry))
                                                                 (call-interactively #'h/set-nickname)))
                                                            :help "Set nickname for hyperdrive"
                                                            :active (h/writablep drive)
                                                            :label
                                                            (format-message "Set nickname: `%s'"
                                                                            (pcase (alist-get 'name (h/metadata drive))
                                                                              (`nil "none")
                                                                              (it it))))
                                                    "---"
                                                    (vector "Purge"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let ((h/current-entry ,entry))
                                                                 (call-interactively #'h/purge)))
                                                            :help "Purge all local data about hyperdrive")))))
                 (append (list ["Writable" :active nil])
                         (or (list-drives (sort (cl-remove-if-not #'h/writablep (hash-table-values h/hyperdrives))
                                                (lambda (a b)
                                                  (string< (h//format a)
                                                           (h//format b)))))
                             (list ["none" :active nil]))
                         (list "---")
                         (list ["Read-only" :active nil])
                         (or (list-drives (sort (cl-remove-if #'h/writablep (hash-table-values h/hyperdrives))
                                                (lambda (a b)
                                                  (string< (h//format a)
                                                           (h//format b)))))
                             (list ["none" :active nil]))))))
    ("Current"
     :active h/current-entry
     :label (if-let* ((entry h/current-entry))
                (format-message "Current: `%s'"
                                (h//format-entry entry))
              "Current")
     ("Current Drive"
      :active h/current-entry
      :label (if-let* ((entry h/current-entry)
                       (hyperdrive (he/hyperdrive entry)))
                 (format-message "Current Drive `%s'" (h//format hyperdrive))
               "Current Drive")
      ["Find File"
       (lambda ()
         (interactive)
         (h/open
           (h/read-entry
            :hyperdrive (he/hyperdrive h/current-entry)
            :read-version current-prefix-arg)))
       :help "Find a file in hyperdrive"]
      ["View File"
       (lambda ()
         (interactive)
         (h/view-file
          (h/read-entry
           :hyperdrive (he/hyperdrive h/current-entry)
           :read-version current-prefix-arg)))
       :help "View a file in hyperdrive"]
      "---"
      ["Upload File"
       (lambda ()
         (interactive)
         (let* ((filename (read-file-name "Upload file: "))
                (entry (h/read-entry :hyperdrive (he/hyperdrive h/current-entry)
                                     :default-path (file-name-nondirectory filename)
                                     :latest-version t)))
           (h/upload-file filename entry)))
       :active (h/writablep (he/hyperdrive h/current-entry))
       :help "Upload a file to hyperdrive"]
      ["Upload Files"
       (lambda ()
         (interactive)
         (let* ((files (h/read-files))
                (drive (he/hyperdrive h/current-entry))
                (target-dir (h/read-path
                             :hyperdrive drive
                             :prompt "Target directory in `%s'"
                             :default "/")))
           (h/upload-files files drive target-dir)))
       :active (h/writablep (he/hyperdrive h/current-entry))
       :help "Upload files to hyperdrive"]
      ["Mirror" h/mirror
       :active (h/writablep (he/hyperdrive h/current-entry))
       :help "Mirror a directory to hyperdrive"]
      "---"
      ["Petname"
       ;; TODO: Remove this and following workarounds for [INSERT-BUG-HERE] when fixed.
       ;;       This workaround prevents keybindings from displaying in the menu bar.
       (lambda ()
         (interactive)
         (call-interactively #'h/set-petname))
       :help "Set petname for hyperdrive"
       :label
       (format-message "Set petname: `%s'"
                       (pcase (h/petname (he/hyperdrive h/current-entry))
                         (`nil "none")
                         (it it)))]
      ["Nickname" (lambda ()
                    (interactive)
                    (call-interactively #'h/set-nickname))
       :help "Set nickname for hyperdrive"
       :active (h/writablep (he/hyperdrive h/current-entry))
       :label
       (format-message "Set nickname: `%s'"
                       (pcase (alist-get 'name
                                         (h/metadata
                                          (he/hyperdrive
                                           h/current-entry)))
                         (`nil "none")
                         (it it)))]
      "---"
      ["Describe" (lambda ()
                    (interactive)
                    (call-interactively #'h/describe-hyperdrive))
       :help "Display information about hyperdrive"]
      ["Purge" (lambda ()
                 (interactive)
                 (call-interactively #'h/purge))
       :help "Purge all local data about hyperdrive"])
     ("Current File/Directory"
      :label (format-message "Current %s: `%s'"
                             (if (h//entry-directory-p h/current-entry)
                                 "Directory"
                               "File")
                             (h//format-path (he/path
                                              h/current-entry)))
      ["Refresh" (lambda ()
                   (interactive)
                   (call-interactively #'revert-buffer))
       :help "Revert current hyperdrive file/directory"]
      ["Up to Parent" (lambda ()
                        (interactive)
                        (call-interactively #'h/up))
       :active (h/parent h/current-entry)
       :help "Open parent directory"]
      ("Sort Directory"
       :active (eq major-mode 'h/dir-mode)
       ["By Name" (lambda ()
                    (interactive)
                    (h/dir-sort
                     (h/dir-toggle-sort-direction
                      'name h/directory-sort)))
        :suffix (pcase-let ((`(,column . ,direction) h/directory-sort))
                  (and (eq 'name column)
                       (format " (%s)" (if (eq 'ascending direction) "v" "^"))))
        :help "Sort directory by name"]
       ["By Size" (lambda ()
                    (interactive)
                    (h/dir-sort
                     (h/dir-toggle-sort-direction
                      'size h/directory-sort)))
        :suffix (pcase-let ((`(,column . ,direction) h/directory-sort))
                  (and (string= 'size column)
                       (format " (%s)" (if (eq 'ascending direction) "v" "^"))))
        :help "Sort directory by size"]
       ["By Last Modified Time" (lambda ()
                                  (interactive)
                                  (h/dir-sort
                                   (h/dir-toggle-sort-direction
                                    'mtime h/directory-sort)))
        :suffix (pcase-let ((`(,column . ,direction) h/directory-sort))
                  (and (string= 'mtime column)
                       (format " (%s)" (if (eq 'ascending direction) "v" "^"))))
        :help "Sort directory by last modified time"])
      ["Copy URL" (lambda ()
                    (interactive)
                    (call-interactively #'h/copy-url))
       :help "Copy URL of current file/directory"]
      ["Delete" (lambda ()
                  (interactive)
                  (call-interactively #'h/delete))
       :active (pcase-let (((cl-struct hyperdrive-entry hyperdrive version) h/current-entry))
                 (and (not (eq major-mode 'h/dir-mode))
                      (not version)
                      (h/writablep hyperdrive)))
       :help "Delete current file/directory"]
      ;; TODO: Add command to download whole directories
      ["Download" (lambda ()
                    (interactive)
                    (call-interactively #'h/download))
       :active (not (eq major-mode 'h/dir-mode))
       :help "Download current file"])
     ("Selected"
      :label (let ((entry-at-point (h/dir--entry-at-point)))
               (format-message "Selected %s: `%s'"
                               (if (h//entry-directory-p entry-at-point)
                                   "Directory"
                                 "File")
                               (he/name entry-at-point)))
      :visible (and (eq major-mode 'h/dir-mode)
                    (h/dir--entry-at-point))
      ["Download" (lambda ()
                    (interactive)
                    (call-interactively #'h/download))
       :active (and-let* ((entry-at-point (h/dir--entry-at-point)))
                 (not (h//entry-directory-p entry-at-point)))
       ;; TODO: Change to "file/directory" when it's possible to download a whole directory
       :help "Download file at point"]
      ["Delete" (lambda ()
                  (interactive)
                  (call-interactively #'h/delete))
       :active (let ((selected-entry (h/dir--entry-at-point)))
                 (and (h/writablep
                       (he/hyperdrive h/current-entry))
                      (not (eq selected-entry h/current-entry))
                      ;; TODO: Add `hyperdrive--parent-entry-p'
                      (not (string= ".." (alist-get 'display-name
                                                    (he/etc selected-entry))))))
       :help "Delete file/directory at point"]
      ["Copy URL" (lambda ()
                    (interactive)
                    (call-interactively #'h/dir-copy-url))
       :help "Copy URL of file/directory at point"]
      ["Open" (lambda ()
                (interactive)
                (call-interactively #'h/dir-find-file))
       :help "Open file/directory at point"]
      ["View" (lambda ()
                (interactive)
                (call-interactively #'h/dir-view-file))
       :active (and-let* ((entry-at-point (h/dir--entry-at-point)))
                 (not (h//entry-directory-p entry-at-point)))
       :help "View file at point"])
     ("Version"
      :label (format "Version (%s)"
                     (or (he/version h/current-entry)
                         "latest"))
      ["Previous Version" (lambda ()
                            (interactive)
                            (call-interactively #'h/open-previous-version))
       :active (he/previous h/current-entry :cache-only t)
       :label (concat "Previous Version"
                      (pcase-exhaustive (he/previous h/current-entry :cache-only t)
                        ('unknown (format " (?)"))
                        ('nil nil)
                        ((cl-struct hyperdrive-entry version)
                         (format " (%s)" version))))
       :help "Open previous version"]
      ["Next Version" (lambda ()
                        (interactive)
                        (call-interactively #'h/open-next-version))
       :active (and (he/version h/current-entry)
                    (he/next h/current-entry))
       :label (concat "Next Version"
                      (and-let* ((entry h/current-entry)
                                 (next-entry (he/next entry))
                                 ;; Don't add ": latest" if we're already at the latest version
                                 ((not (eq entry next-entry)))
                                 (display-version (if-let ((next-version (he/version next-entry)))
                                                      (number-to-string next-version)
                                                    "latest")))
                        (format " (%s)" display-version)))
       :help "Open next version"]
      ["Open Specific Version" (lambda ()
                                 (interactive)
                                 (call-interactively #'h/open-at-version))
       :help "Open specific version"]
      ["Version History" (lambda ()
                           (interactive)
                           (call-interactively #'h/history))
       :help "Open version history"]))
    "---"
    ("Bookmark"
     ["Bookmark Jump" h/bookmark-jump
      :help "Jump to hyperdrive bookmark"]
     ["Bookmark List" h/bookmark-list
      :help "List hyperdrive bookmarks"]
     ["Bookmark Set" bookmark-set
      :active h/current-entry
      :help "Create a new hyperdrive bookmark"])
    "---"
    ["Customize" h/customize
     :help "Customize hyperdrive options"]
    ["User Manual" h/info-manual
     :help "Open hyperdrive.el info manual"]))

;;;###autoload
(define-minor-mode hyperdrive-menu-bar-mode "Show \"Hyperdrive\" menu bar."
  :global t :group 'hyperdrive
  (if h/menu-bar-mode
      ;; Inspired by https://utcc.utoronto.ca/~cks/space/blog/programming/EmacsEasyMenuAndMenubarOrder
      (define-key-after global-map [menu-bar hyperdrive]
        (easy-menu-binding
         (easy-menu-create-menu "Hyperdrive" h/menu-bar-menu) "Hyperdrive")
        "Tools")
    (define-key global-map [menu-bar hyperdrive] nil)))

;;;;; Miscellaneous commands

;;;###autoload
(defun hyperdrive-customize ()
  "Customize Hyperdrive options."
  (interactive)
  (customize-group 'hyperdrive))

;;;###autoload
(defun hyperdrive-info-manual ()
  "Open Hyperdrive info manual."
  (interactive)
  (info "(hyperdrive) Top"))

;;;;; Markdown link support

(defun h//markdown-follow-link (url)
  "Follow URL.
For use in `markdown-follow-link-functions'."
  (pcase (url-type (url-generic-parse-url url))
    ((and `nil (guard (and h/mode h/current-entry)))
     (h/open (h//markdown-url-entry url))
     t)
    (_ nil)))

(defun h//markdown-url-entry (url)
  "Return hyperdrive entry for URL in `markdown-mode' buffer.
Intended for relative (i.e. non-full) URLs."
  (pcase-let (((cl-struct url filename) (url-generic-parse-url url))
              ((cl-struct hyperdrive-entry hyperdrive path)
               h/current-entry))
    ;; NOTE: Depending on the resolution of
    ;; <https://github.com/jrblevin/markdown-mode/issues/805>, we may
    ;; want to URL-decode paths.  For now, we won't.
    (he/create
     :hyperdrive hyperdrive
     :path (expand-file-name filename (file-name-directory path))
     ;; FIXME: Target.
     ;; :etc `((target . ,FOO))
     )))

;;;###autoload
(with-eval-after-load 'markdown-mode
  (when (boundp 'markdown-follow-link-functions)
    (cl-pushnew #'hyperdrive--markdown-follow-link markdown-follow-link-functions)))

;;;;; `find-file-at-point' (`ffap') support

(eval-when-compile (require 'ffap))

(with-eval-after-load 'ffap
  (setf ffap-url-regexp
        (if ffap-url-regexp
            (rx-to-string `(or ,ffap-url-regexp (seq bos "hyper://")))
          (rx bos "hyper://"))))

;;;;; Embark integration

(defvar embark-general-map)
(defvar embark-keymap-alist)

(declare-function h/menu-hyperdrive "hyperdrive-menu" nil t)

(with-eval-after-load 'embark
  (defvar-keymap h/embark-hyperdrive-map
    :doc "Keymap for Embark actions on hyperdrives."
    :parent embark-general-map
    "h" #'h/menu-hyperdrive
    "p" #'h/set-petname
    "n" #'h/set-nickname)

  (add-to-list 'embark-keymap-alist '(hyperdrive . h/embark-hyperdrive-map)))

;;;;; Installation

(defvar h/gateway-urls-and-hashes
  '((gnu/linux
     ( :url "https://codeberg.org/USHIN/hyper-gateway-ushin/releases/download/v3.8.0/hyper-gateway-ushin-linux"
       :sha256 "8ff669bd378e88a3c80d65861f4088071852afaedf7bba56c88c1a162ed9e4f3")
     ( :url "https://git.sr.ht/~ushin/hyper-gateway-ushin/refs/download/v3.8.0/hyper-gateway-linux-v3.8.0"
       :sha256 ""))
    (darwin
     ( :url "https://codeberg.org/USHIN/hyper-gateway-ushin/releases/download/v3.8.0/hyper-gateway-ushin-macos"
       :sha256 "22f6131f48d740f429690f16baac19b20a2211250360a89580db95415398d03c")
     ( :url "https://git.sr.ht/~ushin/hyper-gateway-ushin/refs/download/v3.8.0/hyper-gateway-macos-v3.8.0"
       :sha256 ""))
    (windows-nt
     ( :url "https://codeberg.org/USHIN/hyper-gateway-ushin/releases/download/v3.8.0/hyper-gateway-ushin-windows.exe"
       :sha256 "c347255d3fc5e6499fc10bea4d20e62798fb5968960dbbe26d507d11688326bb")
     ( :url "https://git.sr.ht/~ushin/hyper-gateway-ushin/refs/download/v3.8.0/hyper-gateway-windows-v3.8.0.exe"
       :sha256 "")))
  "Alist mapping `system-type' to URLs where hyper-gateway-ushin can be downloaded.")

;;;###autoload
(defun h/install (&optional forcep)
  "Download and install hyper-gateway-ushin.
If FORCEP, don't prompt for confirmation before downloading."
  (interactive (list current-prefix-arg))
  (when h/install-in-progress-p
    (h/error "Installation of gateway already in progress"))
  (declare-function h//hyper-gateway-ushin-path "hyperdrive-lib")
  (unless forcep
    (when (h//hyper-gateway-ushin-path)
      (unless (yes-or-no-p "Download and reinstall/upgrade hyper-gateway-ushin? ")
        (user-error "Not downloading; aborted"))))
  (let ((urls-and-hashes (alist-get system-type h/gateway-urls-and-hashes)))
    (cl-labels
        ((try ()
           (if-let ((url-and-hash (pop urls-and-hashes)))
               (pcase-let (((map :url :sha256) url-and-hash))
                 ;; TODO: Prompt before downloading.
                 (download url sha256))
             (setf h/install-in-progress-p nil)
             (h/error "Downloading failed; no more mirrors available")))
         (head-size (url)
           (when-let ((response (plz 'head url :as 'response)))
             (cl-parse-integer
              (alist-get 'content-length (plz-response-headers response)))))
         (download (url sha256)
           (plz 'get url :as 'file
             :then (lambda (filename)
                     (check filename sha256))
             :else (lambda (plz-error)
                     (h/message "Trying next source because downloading failed: %S"
                                plz-error)
                     (try)))
           (h/message "Downloading gateway (%s)..."
                      (or (ignore-errors
                            (file-size-human-readable (head-size url)))
                          "unknown size")))
         (check (file-name sha256)
           (if (with-temp-buffer
                 (insert-file-contents-literally file-name)
                 (equal sha256 (secure-hash 'sha256 (current-buffer))))
               (then file-name)
             (try)))
         (then (file-name)
           (let ((destination-name
                  (expand-file-name "hyper-gateway-ushin" h/gateway-directory)))
             (when (file-exists-p destination-name)
               (move-file-to-trash destination-name))
             (unless (file-directory-p h/gateway-directory)
               (mkdir h/gateway-directory t))
             (rename-file file-name destination-name)
             (chmod destination-name #o755))
           (setf h/install-in-progress-p nil)
           (h/message "Gateway installed.  Try \\[%s]"
                      (if (h//gateway-ready-p)
                          "hyperdrive-restart"
                        "hyperdrive-start"))))
      (setf h/install-in-progress-p t)
      (try))))

(defun h/restart ()
  "Restart the gateway."
  (interactive)
  (h/message "Restarting gateway...")
  (when (or (h/gateway-live-p) (h//gateway-ready-p))
    (h/stop))
  (with-timeout (5 (h/message "Timed out waiting for gateway to stop"))
    (cl-loop while (h/gateway-live-p)
             do (sleep-for 0.2)))
  (h/start))

;; (defun h//gateway-appears-valid-p ()
;;   "Return non-nil if a local installation of the gateway appears valid.
;; That is, if an executable file exists at the expected location
;; with an expected hash."
;;   (when-let ((file-name (h//hyper-gateway-ushin-path)))
;;     (let* ((file-hash (with-temp-buffer
;;                          (insert-file-contents-literally file-name)
;;                          (secure-hash 'sha256 (current-buffer))))
;;            (urls-and-hashes (alist-get system-type h/gateway-urls-and-hashes)))
;;       (cl-loop for pair in urls-and-hashes
;;                for expected-hash = (plist-get pair :sha256)
;;                thereis (equal expected-hash file-hash)))))

;;;; Footer

(provide 'hyperdrive)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive.el ends here
