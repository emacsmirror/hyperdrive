;;; hyperdrive.el --- P2P filesystem  -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023 USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <~ushin/ushin@lists.sr.ht>
;; Created: 2022
;; Version: 0.3-pre
;; Package-Requires: ((emacs "27.1") (map "3.0") (compat "29.1.4.0") (plz "0.7") (persist "0.5") (taxy-magit-section "0.12.1") (transient "0.4.4"))
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

;; TODO[#A]: Command to rename paths.

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

;; TODO: Add user option to start the gateway without systemd (run as
;; Emacs subprocess, or other script)

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
  (interactive (list (hyperdrive-complete-hyperdrive :force-prompt t)))
  (when (yes-or-no-p (format "Delete local copy of hyperdrive (data will likely not be recoverable—see manual): «%s»? "
                             (hyperdrive--format-hyperdrive hyperdrive)))
    (hyperdrive-purge-no-prompt hyperdrive
      :then (lambda (_response)
              (hyperdrive-message "Purged drive: %s" (hyperdrive--format-hyperdrive hyperdrive)))
      :else (lambda (plz-error)
              (hyperdrive-error "Unable to purge drive: %s %S" (hyperdrive--format-hyperdrive hyperdrive) plz-error)))))

;;;###autoload
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

;;;###autoload
(cl-defun hyperdrive-set-nickname (nickname hyperdrive &key (then #'ignore))
  "Set HYPERDRIVE's NICKNAME.
Returns HYPERDRIVE.

Asynchronous callback calls THEN with the updated hyperdrive as
its only argument.

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
                    (hyperdrive--fill-latest-version hyperdrive headers)
                    (hyperdrive-persist hyperdrive)
                    (funcall then hyperdrive))))
      (setf (alist-get 'name (hyperdrive-metadata hyperdrive)) nickname)
      (hyperdrive-put-metadata hyperdrive
        :then (pcase-lambda ((cl-struct plz-response headers))
                (hyperdrive--fill-latest-version hyperdrive headers)
                (hyperdrive-persist hyperdrive)
                (funcall then hyperdrive))))
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

(defvar-local hyperdrive-mode--state nil
  "Previous state of buffer before `hyperdrive-mode' was activated.
Intended to be passed to `buffer-local-restore-state'.")

;;;###autoload
(define-minor-mode hyperdrive-mode
  ;; TODO: Consider moving hyperdrive-mode definition to
  ;; hyperdrive-lib.el.  (Since it's used in multiple files.)
  "Minor mode for buffers opened from hyperdrives."
  :global nil
  :interactive nil
  :group 'hyperdrive
  :lighter " hyperdrive"
  :keymap '(([remap revert-buffer-quick] . hyperdrive-revert-buffer-quick)
            ([remap dired-jump] .  hyperdrive-up))
  (if hyperdrive-mode
      (progn
        (setq-local hyperdrive-mode--state
                    (buffer-local-set-state
                     revert-buffer-function #'hyperdrive-revert-buffer
                     bookmark-make-record-function #'hyperdrive-bookmark-make-record
                     write-contents-functions (cl-adjoin #'hyperdrive--write-contents write-contents-functions)
                     ;; TODO: Modify buffer-local value of `save-some-buffers-action-alist'
                     ;; to allow diffing modified buffer with hyperdrive file
                     buffer-offer-save t))
        (add-hook 'after-change-major-mode-hook
                  #'hyperdrive--hack-write-contents-functions nil 'local)
        ;; TODO: Consider checking for existing advice before adding our own.
        (advice-add #'org-insert-link :after #'hyperdrive--org-insert-link-after-advice))
    (buffer-local-restore-state hyperdrive-mode--state)
    (remove-hook 'after-change-major-mode-hook
                 #'hyperdrive--hack-write-contents-functions 'local)
    ;; FIXME: Only remove advice when all hyperdrive-mode buffers are killed.
    ;; (advice-remove #'org-insert-link #'hyperdrive--org-insert-link)
    ))
;; Making it permanent-local keeps the minor mode active even if the
;; user changes the major mode, so the buffer can still be saved back
;; to the hyperdrive.
(put 'hyperdrive-mode 'permanent-local t)

(defun hyperdrive--hack-write-contents-functions ()
  "Hack `write-contents-functions' for `hyperdrive-mode' in current buffer.
Ensures that hyperdrive buffers can still be saved after the
major mode changes (which resets `write-contents-functions' by
calling `kill-all-local-variables')."
  (cl-pushnew #'hyperdrive--write-contents write-contents-functions))
(put 'hyperdrive--hack-write-contents-functions 'permanent-local-hook t)

;;;###autoload
(defun hyperdrive-find-file (entry)
  "Find hyperdrive ENTRY.
Interactively, prompt for known hyperdrive and path.
With universal prefix argument \\[universal-argument], prompt for version."
  (interactive (list (hyperdrive-read-entry :read-version current-prefix-arg)))
  (hyperdrive-open entry))

;;;###autoload
(defun hyperdrive-view-file (entry)
  "View ENTRY in `view-mode', returning to previous buffer when done.
Interactively, prompt for known hyperdrive and path.
With universal prefix argument \\[universal-argument], prompt for version."
  ;; TODO: Stay in `view-mode' after
  ;; `hyperdrive-previous-version'/`hyperdrive-next-version'. This may
  ;; require another minor mode.
  (interactive (list (hyperdrive-read-entry :read-version current-prefix-arg)))
  (hyperdrive-open entry
    ;; `view-buffer' checks the mode-class symbol property of
    ;; `major-mode' and avoids putting directory buffers in `view-mode'.
    :createp nil :then (lambda () (view-buffer (current-buffer)))))

;;;###autoload
(defun hyperdrive-open-url (url)
  "Open hyperdrive URL."
  (interactive (list (hyperdrive-read-url :prompt "Open hyperdrive URL")))
  (hyperdrive-open (hyperdrive-url-entry url)))

;;;###autoload
(cl-defun hyperdrive-delete (entry &key (then #'ignore) (else #'ignore))
  "Delete ENTRY, then call THEN with response.
Call ELSE with `plz-error' struct if request fails.
Interactively, delete current file ENTRY or ENTRY at point in a
directory.  Otherwise, or with universal prefix argument
\\[universal-argument], prompt for ENTRY."
  (declare (indent defun))
  (interactive
   (let* ((entry (hyperdrive--context-entry :latest-version t))
          (description (hyperdrive--format-entry entry))
          (buffer (current-buffer)))
     (when (and (hyperdrive--entry-directory-p entry)
                (or (eq entry hyperdrive-current-entry)
                    (string= "../" (alist-get 'display-name (hyperdrive-entry-etc entry)))))
       (hyperdrive-user-error "Won't delete from within"))
     (when (and (yes-or-no-p (format "Delete «%s»? " description))
                (or (not (hyperdrive--entry-directory-p entry))
                    (yes-or-no-p (format "Recursively delete «%s»? " description))))
       (list entry
             :then (lambda (_)
                     (when (and (buffer-live-p buffer)
                                (eq 'hyperdrive-dir-mode (buffer-local-value 'major-mode buffer)))
                       (with-current-buffer buffer
                         (revert-buffer)))
                     (hyperdrive-message "Deleted: «%s» (Deleted files can be accessed from prior versions of the hyperdrive.)" description))
             :else (lambda (plz-error)
                     (hyperdrive-message "Unable to delete «%s»: %S" description plz-error))))))
  (hyperdrive-api 'delete (hyperdrive-entry-url entry)
    :as 'response
    :then (lambda (response)
            (pcase-let* (((cl-struct plz-response headers) response)
                         ((map etag) headers)
                         (nonexistent-entry (hyperdrive-copy-tree entry t)))
              (unless (hyperdrive--entry-directory-p entry)
                ;; FIXME: hypercore-fetch bug doesn't update version
                ;; number when deleting a directory.
                (setf (hyperdrive-entry-version nonexistent-entry) (string-to-number etag))
                (hyperdrive--fill-latest-version (hyperdrive-entry-hyperdrive entry) headers)
                (hyperdrive-update-nonexistent-version-range nonexistent-entry))
              ;; Since there's no way for `hyperdrive--write-contents' to run when
              ;; `buffer-modified-p' returns nil, this is a workaround to ensure that
              ;; `save-buffer' re-saves files after they've been deleted.
              (dolist (buf (match-buffers (lambda (buf deleted-entry)
                                            (when-let ((current-entry (buffer-local-value 'hyperdrive-current-entry buf)))
                                              (hyperdrive-entry-equal-p current-entry deleted-entry)))
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
   (pcase-let* ((entry (hyperdrive--context-entry))
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
  (when (or (not (file-exists-p filename))
            (yes-or-no-p (format "File %s already exists; overwrite anyway? " (expand-file-name filename))))
    (when (file-exists-p filename)
      ;; plz.el will not overwrite existing files: ensure there's no file there.
      (delete-file filename))
    (hyperdrive-api 'get url :as `(file ,filename))))

;;;###autoload
(defun hyperdrive-write-buffer (entry &optional overwritep)
  "Write current buffer to new hyperdrive ENTRY.
If file already exists and OVERWRITEP is nil, prompt the user to
overwrite.

With universal prefix argument \\[universal-argument], overwrite
without prompting.

This function is for interactive use only; for non-interactive
use, see `hyperdrive-write'."
  (interactive (list (hyperdrive-read-entry :predicate #'hyperdrive-writablep
                                            :default-path (when hyperdrive-current-entry
                                                            (hyperdrive-entry-path hyperdrive-current-entry))
                                            :latest-version t)
                     current-prefix-arg))
  (unless (or overwritep (not (hyperdrive-entry-at nil entry)))
    (unless (y-or-n-p
	     (format "File %s exists; overwrite?" (hyperdrive--format-entry entry)))
      (hyperdrive-user-error "Canceled"))
    (when-let ((buffers (match-buffers (hyperdrive--buffer-for-entry entry))))
      (unless (y-or-n-p
	       (format "A buffer is visiting %s; proceed?" (hyperdrive--format-entry entry)))
        (hyperdrive-user-error "Aborted"))
      ;; TODO: In BUFFERS, when user attempts to modify the buffer,
      ;; offer warning like "FILE has been modified in hyperdrive; are
      ;; you sure you want to edit this buffer?"
      ;; TODO: Replace `match-buffers' above with `cl-find-if' if we don't
      ;; end up adding a buffer-local variable to each buffer to
      ;; indicate that the file in the hyperdrive has been modified.
      (ignore buffers)
      ))
  (pcase-let (((cl-struct hyperdrive-entry hyperdrive name) entry)
              (url (hyperdrive-entry-url entry))
              (buffer (current-buffer)))
    (hyperdrive-write entry
      :body (without-restriction
              (buffer-substring-no-properties (point-min) (point-max)))
      :then (lambda (response)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (unless hyperdrive-mode
                    (hyperdrive--clean-buffer)
                    (when hyperdrive-honor-auto-mode-alist
                      (let ((buffer-file-name (hyperdrive-entry-name entry)))
                        (set-auto-mode)))
                    (hyperdrive-mode))
                  ;; NOTE: `hyperdrive-fill-latest-version' must come before
                  ;; `hyperdrive--fill' because the latter calls
                  ;; `hyperdrive-update-existent-version-range' internally.
                  (hyperdrive-fill-latest-version hyperdrive)
                  (hyperdrive--fill entry (plz-response-headers response))
                  ;; PUT responses only include ETag and Last-Modified
                  ;; headers, so we need to set other entry metadata manually.
                  ;; FIXME: For large buffers, `buffer-size' returns a different
                  ;; value than hyper-gateway's Content-Length header.
                  (setf (hyperdrive-entry-size entry) (buffer-size))
                  ;; FIXME: Will entry type ever be anything besides text/plain?
                  ;;        /.well-known/host-meta.json ?
                  (setf (hyperdrive-entry-type entry) "text/plain; charset=utf-8")
                  (setq-local hyperdrive-current-entry entry)
                  (setf buffer-file-name nil)
                  (rename-buffer
                   (hyperdrive--format-entry entry hyperdrive-buffer-name-format)
                   'unique)
                  (set-buffer-modified-p nil)
                  ;; Update the visited file modtime so undo commands
                  ;; correctly set the buffer-modified flag.  We just
                  ;; use `current-time' here since it's good enough
                  ;; and lets us avoid making another request for
                  ;; metadata.
                  (set-visited-file-modtime (current-time))))
              (hyperdrive-message "Wrote: %S to \"%s\"" name url))
      :else (lambda (plz-error)
              (hyperdrive-message "Unable to write: %S: %S" name plz-error)))
    (hyperdrive-message "Saving to \"%s\"..." url)
    ;; TODO: Reload relevant hyperdrive-dir buffers after writing buffer (if ewoc buffers display version, then possibly all ewoc buffers for a given hyperdrive should be reloaded)
    ))

(defun hyperdrive--write-contents ()
  "Call `hyperdrive-write-buffer' for the current buffer.
To be used in `write-contents-functions'."
  (cl-assert hyperdrive-mode)
  (hyperdrive-write-buffer hyperdrive-current-entry t))

(defun hyperdrive-copy-url (entry)
  "Save hyperdrive ENTRY's URL to the kill ring.
Interactively, uses `hyperdrive-current-entry', from either a
hyperdrive directory listing or a `hyperdrive-mode' file buffer."
  (declare (modes hyperdrive-mode))
  (interactive (list hyperdrive-current-entry))
  (let ((url (hyperdrive-entry-url entry)))
    (kill-new url)
    (hyperdrive-message "%s" url)))

(cl-defun hyperdrive-up (entry &key (then nil then-set-p))
  "Go up to parent directory of ENTRY.
Interactively, use the `hyperdrive-current-entry'.  If THEN, pass
it to `hyperdrive-open'."
  (declare (modes hyperdrive-mode))
  (interactive (progn
                 (unless (and hyperdrive-mode hyperdrive-current-entry)
                   (user-error "Not a hyperdrive buffer"))
                 (list hyperdrive-current-entry)))
  (if-let ((parent (hyperdrive-parent entry)))
      ;; TODO: Go to entry in parent directory.
      (if then-set-p
          (hyperdrive-open parent :then then)
        ;; Allow default callback to be used.
        (hyperdrive-open parent))
    (hyperdrive-user-error "At root directory")))

(defvar-keymap hyperdrive-up-map
  :doc "Keymap to repeat `hyperdrive-up'.  Used in `repeat-mode'."
  :repeat t
  "j"   #'hyperdrive-up
  "C-j" #'hyperdrive-up)

(defun hyperdrive-open-previous-version (entry)
  "Open previous version of ENTRY."
  (declare (modes hyperdrive-mode))
  (interactive (list hyperdrive-current-entry))
  (if-let ((previous-entry (hyperdrive-entry-previous entry)))
      (hyperdrive-open previous-entry)
    (hyperdrive-message (substitute-command-keys "%s does not exist at version %s. Try \\[hyperdrive-history]")
                        (hyperdrive--format-entry entry "[%H] %p")
                        (1- (car (hyperdrive-entry-version-range entry))))))

(defun hyperdrive-open-next-version (entry)
  "Open next version of ENTRY."
  (declare (modes hyperdrive-mode))
  (interactive (list hyperdrive-current-entry))
  (pcase-exhaustive (hyperdrive-entry-next entry)
    ((and (pred (eq entry)) next-entry)
     ;; ENTRY already at latest version: open and say `revert-buffer'.
     (hyperdrive-open next-entry)
     (hyperdrive-message
      "Already at latest version of entry; consider reverting buffer with %s to check for newer versions"
      (substitute-command-keys
       (if (fboundp 'revert-buffer-quick)
           "\\[revert-buffer-quick]"
         "\\[revert-buffer]"))))
    ('nil ;; Known nonexistent: suggest `hyperdrive-history'.
     (hyperdrive-message (substitute-command-keys
                          "Entry deleted after this version. Try \\[hyperdrive-history]")))
    ('unknown ;; Unknown existence: suggest `hyperdrive-history'.
     (hyperdrive-message (substitute-command-keys
                          "Next version unknown. Try \\[hyperdrive-history]")))
    ((and (pred hyperdrive-entry-p) next-entry)
     (hyperdrive-open next-entry))))

(defun hyperdrive-open-at-version (entry version)
  "Open ENTRY at VERSION.
Nil VERSION means open the entry at its hyperdrive's latest version."
  (declare (modes hyperdrive-mode))
  (interactive (let ((entry hyperdrive-current-entry))
                 (list entry (hyperdrive-read-version
                              :hyperdrive (hyperdrive-entry-hyperdrive entry)
                              :prompt (format "Open «%s» at version (leave blank for latest version)"
                                              (hyperdrive--format-entry entry))))))
  (if-let ((latest-entry (hyperdrive-entry-at version entry)))
      (hyperdrive-open latest-entry)
    (hyperdrive-message (substitute-command-keys "%s does not exist at version %s. Try \\[hyperdrive-history]")
                        (hyperdrive--format-entry
                         entry hyperdrive-default-entry-format-without-version)
                        version)))

;;;; Bookmark support

;; TODO: Display entry description instead of full URL in bookmark list view.
(require 'bookmark)

(defun hyperdrive-bookmark-make-record ()
  "Return a bookmark record for current hyperdrive buffer.
Works in `hyperdrive-mode' and `hyperdrive-dir-mode' buffers."
  (let ((bookmark (bookmark-make-record-default 'no-file)))
    (setf (alist-get 'handler bookmark) #'hyperdrive-bookmark-handler
          (alist-get 'location bookmark) (hyperdrive-entry-url hyperdrive-current-entry))
    (cons (format "hyperdrive: %s" (hyperdrive--format-entry hyperdrive-current-entry)) bookmark)))

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
             (append bookmark `((buffer . ,(current-buffer)))))
            (pop-to-buffer (current-buffer) '(display-buffer-same-window)))))
(put 'hyperdrive-bookmark-handler 'bookmark-handler-type "hyperdrive")

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
After successful upload, call THEN.  When QUEUE, use it."
  (declare (indent defun))
  (interactive (let ((filename (read-file-name "Upload file: ")))
                 (list filename
                       (hyperdrive-read-entry :predicate #'hyperdrive-writablep
                                              :default-path (file-name-nondirectory filename)
                                              :latest-version t))))
  (let ((url (hyperdrive-entry-url entry))
        (last-modified (let ((system-time-locale "C"))
                         (format-time-string "%Y-%m-%dT%T.%3NZ"
                                             ;; "%a, %-d %b %Y %T %Z"
                                             (file-attribute-modification-time
                                              (file-attributes filename)) t))))
    (hyperdrive-api 'put url :queue queue
      :body `(file ,filename)
      :headers `(("Last-Modified" . ,last-modified))
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
   (let* ((files (hyperdrive-read-files))
          (hyperdrive (hyperdrive-complete-hyperdrive :predicate #'hyperdrive-writablep
                                                      :force-prompt current-prefix-arg))
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
                :limit hyperdrive-queue-limit
                :finally (lambda ()
                           ;; FIXME: Offer more informative message in case of errors?
                           (hyperdrive-open (hyperdrive-entry-create :hyperdrive hyperdrive
                                                                     :path target-directory))
                           (hyperdrive-message "Uploaded %s files." (length files))))))
    (dolist (file files)
      (let* ((path (file-name-concat target-directory (file-name-nondirectory file)))
             (entry (hyperdrive-entry-create :hyperdrive hyperdrive :path path)))
        ;; TODO: Handle failures? Retry?
        (hyperdrive-upload-file file entry :queue queue :then #'ignore)))
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

(defun hyperdrive-url-loader (parsed-url)
  "Retrieve URL synchronously.
PARSED-URL must be a URL-struct like the output of
`url-generic-parse-url'.

The return value of this function is the retrieval buffer."
  (cl-check-type parsed-url url "Need a pre-parsed URL.")
  (let* ((url (url-recreate-url parsed-url))
         ;; response-buffer will contain the loaded HTML, and will be deleted at the end of `eww-render'.
         (response-buffer (hyperdrive-api 'get url :as 'buffer)))
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

(puthash "hyper" '(name "hyper" loader hyperdrive-url-loader
                        ;; Expand relative paths against host
                        expand-file-name url-default-expander)
         url-scheme-registry)

(defvar eww-use-browse-url)
(when (version<= "28.1" emacs-version)
  (require 'eww)
  (setf eww-use-browse-url
        (if eww-use-browse-url
            (rx-to-string `(or ,eww-use-browse-url (seq bos "hyper://")))
          (rx bos "hyper://"))))

;;;; `kill-buffer-query-functions' integration

(defun hyperdrive--kill-buffer-possibly-save (buffer)
  "Ask whether to kill modified hyperdrive file BUFFER."
  ;; Mostly copied from `kill-buffer--possibly-save'.
  (cl-assert (and hyperdrive-mode hyperdrive-current-entry))
  (let ((response
         (cadr
          (if (< emacs-major-version 28)
              (read-multiple-choice
               (format "Hyperdrive file %s modified; kill anyway?"
                       (hyperdrive--format-entry hyperdrive-current-entry))
               '((?y "yes" "kill buffer without saving")
                 (?n "no" "exit without doing anything")
                 (?s "save and then kill" "save the buffer and then kill it")))
            (with-suppressed-warnings ((free-vars use-short-answers))
              (compat-call read-multiple-choice
                           (format "Hyperdrive file %s modified; kill anyway?"
                                   (hyperdrive--format-entry hyperdrive-current-entry))
                           '((?y "yes" "kill buffer without saving")
                             (?n "no" "exit without doing anything")
                             (?s "save and then kill" "save the buffer and then kill it"))
                           nil nil (and (not use-short-answers)
                                        (not (when (fboundp 'use-dialog-box-p)
                                               (with-no-warnings
                                                 (use-dialog-box-p)))))))))))
    (if (equal response "no")
        nil
      (unless (equal response "yes")
        (with-current-buffer buffer
          (save-buffer)))
      t)))

(defun hyperdrive-kill-buffer-query-function ()
  "Ask before killing an unsaved hyperdrive file buffer."
  (if (and hyperdrive-mode
           hyperdrive-current-entry
           (not (hyperdrive--entry-directory-p hyperdrive-current-entry))
           (buffer-modified-p))
      (hyperdrive--kill-buffer-possibly-save (current-buffer))
    t))

(cl-pushnew #'hyperdrive-kill-buffer-query-function kill-buffer-query-functions)

;;;;; `easy-menu' integration

(defvar hyperdrive-menu-bar-menu
  '("Hyperdrive"
    ("Gateway"
     :label
     (format "Gateway (%s)" (if (hyperdrive-status) "on" "off"))
     ["Start Gateway" hyperdrive-start
      :help "Start hyper-gateway"]
     ["Stop Gateway" hyperdrive-stop
      :help "Stop hyper-gateway"]
     ["Gateway version" hyperdrive-hyper-gateway-version
      :help "Say hyper-gateway version"])
    "---"
    ["Open URL" hyperdrive-open-url
     :help "Load a hyperdrive URL"]
    ["New Drive" hyperdrive-new
     :help "Create a new hyperdrive"]
    ("Drives"
     :active (< 0 (hash-table-count hyperdrive-hyperdrives))
     :label (if (zerop (hash-table-count hyperdrive-hyperdrives))
                "Drives (empty)"
              "Drives")
     :filter (lambda (_)
               (cl-labels ((list-drives (drives)
                             (cl-loop for drive in drives
                                      for entry = (hyperdrive-entry-create :hyperdrive drive)
                                      collect (list (hyperdrive--format-host drive :with-label t)
                                                    (vector "Describe"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let ((hyperdrive-current-entry ,entry))
                                                                 (call-interactively #'hyperdrive-describe-hyperdrive)))
                                                            :help "Display information about hyperdrive")
                                                    (vector "Find File"
                                                            `(lambda ()
                                                               (interactive)
                                                               (hyperdrive-open
                                                                 (hyperdrive-read-entry
                                                                  :hyperdrive ,drive
                                                                  :read-version current-prefix-arg)))
                                                            :help "Find a file in hyperdrive")
                                                    (vector "View File"
                                                            `(lambda ()
                                                               (interactive)
                                                               (hyperdrive-view-file
                                                                (hyperdrive-read-entry
                                                                 :hyperdrive ,drive
                                                                 :read-version current-prefix-arg)))
                                                            :help "View a file in hyperdrive")
                                                    "---"
                                                    (vector "Upload File"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let* ((filename (read-file-name "Upload file: "))
                                                                      (entry (hyperdrive-read-entry :hyperdrive ,drive
                                                                                                    :default-path (file-name-nondirectory filename)
                                                                                                    :latest-version t)))
                                                                 (hyperdrive-upload-file filename entry)))
                                                            :active `(hyperdrive-writablep ,drive)
                                                            :help "Upload a file to hyperdrive")
                                                    (vector "Upload Files"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let* ((files (hyperdrive-read-files))
                                                                      (target-dir (hyperdrive-read-path
                                                                                   :hyperdrive ,drive
                                                                                   :prompt "Target directory in «%s»"
                                                                                   :default "/")))
                                                                 (hyperdrive-upload-files files ,drive
                                                                                          :target-directory target-dir)))
                                                            :active `(hyperdrive-writablep ,drive)
                                                            :help "Upload files to hyperdrive")
                                                    (vector "Mirror" #'hyperdrive-mirror
                                                            ;; TODO: `hyperdrive-mirror''s interactive form will also prompt
                                                            ;; for a drive. After changing `hyperdrive-mirror' to use
                                                            ;; Transient.el, we should pass in the default drive argument.
                                                            :active `(hyperdrive-writablep ,drive)
                                                            :help "Mirror a directory to hyperdrive")
                                                    "---"
                                                    (vector "Petname"
                                                            ;; HACK: We have to unquote the value of the entry because it seems that the filter
                                                            ;; function is called in an environment that doesn't use lexical-binding...?
                                                            ;; TODO: Ask about this and/or file a bug report.
                                                            `(lambda ()
                                                               (interactive)
                                                               (let ((hyperdrive-current-entry ,entry))
                                                                 (call-interactively #'hyperdrive-set-petname)))
                                                            :help "Set petname for hyperdrive"
                                                            :label
                                                            (format "Set petname: «%s»"
                                                                    (pcase (hyperdrive-petname drive)
                                                                      (`nil "none")
                                                                      (it it))))
                                                    (vector "Nickname"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let ((hyperdrive-current-entry ,entry))
                                                                 (call-interactively #'hyperdrive-set-nickname)))
                                                            :help "Set nickname for hyperdrive"
                                                            :active (hyperdrive-writablep drive)
                                                            :label
                                                            (format "Set nickname: «%s»"
                                                                    (pcase (alist-get 'name (hyperdrive-metadata drive))
                                                                      (`nil "none")
                                                                      (it it))))
                                                    "---"
                                                    (vector "Purge"
                                                            `(lambda ()
                                                               (interactive)
                                                               (let ((hyperdrive-current-entry ,entry))
                                                                 (call-interactively #'hyperdrive-purge)))
                                                            :help "Purge all local data about hyperdrive")))))
                 (append (list ["Writable" :active nil])
                         (or (list-drives (sort (cl-remove-if-not #'hyperdrive-writablep (hash-table-values hyperdrive-hyperdrives))
                                                (lambda (a b)
                                                  (string< (hyperdrive--format-host a :with-label t)
                                                           (hyperdrive--format-host b :with-label t)))))
                             (list ["none" :active nil]))
                         (list "---")
                         (list ["Read-only" :active nil])
                         (or (list-drives (sort (cl-remove-if #'hyperdrive-writablep (hash-table-values hyperdrive-hyperdrives))
                                                (lambda (a b)
                                                  (string< (hyperdrive--format-host a :with-label t)
                                                           (hyperdrive--format-host b :with-label t)))))
                             (list ["none" :active nil]))))))
    ("Current"
     :active hyperdrive-current-entry
     :label (if-let* ((entry hyperdrive-current-entry))
                (format "Current: «%s»"
                        (hyperdrive--format-entry entry))
              "Current")
     ("Current Drive"
      :active hyperdrive-current-entry
      :label (if-let* ((entry hyperdrive-current-entry)
                       (hyperdrive (hyperdrive-entry-hyperdrive entry)))
                 (format "Current Drive «%s»" (hyperdrive--format-host hyperdrive :with-label t))
               "Current Drive")
      ["Find File"
       (lambda ()
         (interactive)
         (hyperdrive-open
           (hyperdrive-read-entry
            :hyperdrive (hyperdrive-entry-hyperdrive hyperdrive-current-entry)
            :read-version current-prefix-arg)))
       :help "Find a file in hyperdrive"]
      ["View File"
       (lambda ()
         (interactive)
         (hyperdrive-view-file
          (hyperdrive-read-entry
           :hyperdrive (hyperdrive-entry-hyperdrive hyperdrive-current-entry)
           :read-version current-prefix-arg)))
       :help "View a file in hyperdrive"]
      "---"
      ["Upload File"
       (lambda ()
         (interactive)
         (let* ((filename (read-file-name "Upload file: "))
                (entry (hyperdrive-read-entry :hyperdrive (hyperdrive-entry-hyperdrive hyperdrive-current-entry)
                                              :default-path (file-name-nondirectory filename)
                                              :latest-version t)))
           (hyperdrive-upload-file filename entry)))
       :active (hyperdrive-writablep (hyperdrive-entry-hyperdrive hyperdrive-current-entry))
       :help "Upload a file to hyperdrive"]
      ["Upload Files"
       (lambda ()
         (interactive)
         (let* ((files (hyperdrive-read-files))
                (drive (hyperdrive-entry-hyperdrive hyperdrive-current-entry))
                (target-dir (hyperdrive-read-path
                             :hyperdrive drive
                             :prompt "Target directory in «%s»"
                             :default "/")))
           (hyperdrive-upload-files files drive
                                    :target-directory target-dir)))
       :active (hyperdrive-writablep (hyperdrive-entry-hyperdrive hyperdrive-current-entry))
       :help "Upload files to hyperdrive"]
      ["Mirror" hyperdrive-mirror
       :active (hyperdrive-writablep (hyperdrive-entry-hyperdrive hyperdrive-current-entry))
       :help "Mirror a directory to hyperdrive"]
      "---"
      ["Petname"
       ;; TODO: Remove this and following workarounds for [INSERT-BUG-HERE] when fixed.
       ;;       This workaround prevents keybindings from displaying in the menu bar.
       (lambda ()
         (interactive)
         (call-interactively #'hyperdrive-set-petname))
       :help "Set petname for hyperdrive"
       :label
       (format "Set petname: «%s»"
               (pcase (hyperdrive-petname (hyperdrive-entry-hyperdrive hyperdrive-current-entry))
                 (`nil "none")
                 (it it)))]
      ["Nickname" (lambda ()
                    (interactive)
                    (call-interactively #'hyperdrive-set-nickname))
       :help "Set nickname for hyperdrive"
       :active (hyperdrive-writablep (hyperdrive-entry-hyperdrive hyperdrive-current-entry))
       :label
       (format "Set nickname: «%s»"
               (pcase (alist-get 'name
                                 (hyperdrive-metadata
                                  (hyperdrive-entry-hyperdrive
                                   hyperdrive-current-entry)))
                 (`nil "none")
                 (it it)))]
      "---"
      ["Describe" (lambda ()
                    (interactive)
                    (call-interactively #'hyperdrive-describe-hyperdrive))
       :help "Display information about hyperdrive"]
      ["Purge" (lambda ()
                 (interactive)
                 (call-interactively #'hyperdrive-purge))
       :help "Purge all local data about hyperdrive"])
     ("Current File/Directory"
      :label (format "Current %s: «%s»"
                     (if (hyperdrive--entry-directory-p hyperdrive-current-entry)
                         "Directory"
                       "File")
                     (hyperdrive--format-path (hyperdrive-entry-path
                                               hyperdrive-current-entry)))
      ["Refresh" (lambda ()
                   (interactive)
                   (call-interactively #'revert-buffer))
       :help "Revert current hyperdrive file/directory"]
      ["Up to Parent" (lambda ()
                        (interactive)
                        (call-interactively #'hyperdrive-up))
       :active (hyperdrive-parent hyperdrive-current-entry)
       :help "Open parent directory"]
      ("Sort Directory"
       :active (eq major-mode 'hyperdrive-dir-mode)
       ["By Name" (lambda ()
                    (interactive)
                    (hyperdrive-dir-sort
                     (hyperdrive-dir-toggle-sort-direction
                      'name hyperdrive-directory-sort)))
        :suffix (pcase-let ((`(,column . ,direction) hyperdrive-directory-sort))
                  (when (eq 'name column)
                    (format " (%s)" (if (eq 'ascending direction) "v" "^"))))
        :help "Sort directory by name"]
       ["By Size" (lambda ()
                    (interactive)
                    (hyperdrive-dir-sort
                     (hyperdrive-dir-toggle-sort-direction
                      'size hyperdrive-directory-sort)))
        :suffix (pcase-let ((`(,column . ,direction) hyperdrive-directory-sort))
                  (when (string= 'size column)
                    (format " (%s)" (if (eq 'ascending direction) "v" "^"))))
        :help "Sort directory by size"]
       ["By Last Modified Time" (lambda ()
                                  (interactive)
                                  (hyperdrive-dir-sort
                                   (hyperdrive-dir-toggle-sort-direction
                                    'mtime hyperdrive-directory-sort)))
        :suffix (pcase-let ((`(,column . ,direction) hyperdrive-directory-sort))
                  (when (string= 'mtime column)
                    (format " (%s)" (if (eq 'ascending direction) "v" "^"))))
        :help "Sort directory by last modified time"])
      ["Copy URL" (lambda ()
                    (interactive)
                    (call-interactively #'hyperdrive-copy-url))
       :help "Copy URL of current file/directory"]
      ["Delete" (lambda ()
                  (interactive)
                  (call-interactively #'hyperdrive-delete))
       :active (pcase-let (((cl-struct hyperdrive-entry hyperdrive version) hyperdrive-current-entry))
                 (and (not (eq major-mode 'hyperdrive-dir-mode))
                      (not version)
                      (hyperdrive-writablep hyperdrive)))
       :help "Delete current file/directory"]
      ;; TODO: Add command to download whole directories
      ["Download" (lambda ()
                    (interactive)
                    (call-interactively #'hyperdrive-download))
       :active (not (eq major-mode 'hyperdrive-dir-mode))
       :help "Download current file"])
     ("Selected"
      :label (let ((entry-at-point (hyperdrive-dir--entry-at-point)))
               (format "Selected %s: «%s»"
                       (if (hyperdrive--entry-directory-p entry-at-point)
                           "Directory"
                         "File")
                       (hyperdrive-entry-name entry-at-point)))
      :visible (and (eq major-mode 'hyperdrive-dir-mode)
                    (hyperdrive-dir--entry-at-point))
      ["Download" (lambda ()
                    (interactive)
                    (call-interactively #'hyperdrive-download))
       :active (when-let ((entry-at-point (hyperdrive-dir--entry-at-point)))
                 (not (hyperdrive--entry-directory-p entry-at-point)))
       ;; TODO: Change to "file/directory" when it's possible to download a whole directory
       :help "Download file at point"]
      ["Delete" (lambda ()
                  (interactive)
                  (call-interactively #'hyperdrive-delete))
       :active (let ((selected-entry (hyperdrive-dir--entry-at-point)))
                 (and (hyperdrive-writablep
                       (hyperdrive-entry-hyperdrive hyperdrive-current-entry))
                      (not (eq selected-entry hyperdrive-current-entry))
                      ;; TODO: Add `hyperdrive--parent-entry-p'
                      (not (string= ".." (alist-get 'display-name
                                                    (hyperdrive-entry-etc selected-entry))))))
       :help "Delete file/directory at point"]
      ["Copy URL" (lambda ()
                    (interactive)
                    (call-interactively #'hyperdrive-dir-copy-url))
       :help "Copy URL of file/directory at point"]
      ["Open" (lambda ()
                (interactive)
                (call-interactively #'hyperdrive-dir-find-file))
       :help "Open file/directory at point"]
      ["View" (lambda ()
                (interactive)
                (call-interactively #'hyperdrive-dir-view-file))
       :active (when-let ((entry-at-point (hyperdrive-dir--entry-at-point)))
                 (not (hyperdrive--entry-directory-p entry-at-point)))
       :help "View file at point"])
     ("Version"
      :label (format "Version (%s)"
                     (or (hyperdrive-entry-version hyperdrive-current-entry)
                         "latest"))
      ["Previous Version" (lambda ()
                            (interactive)
                            (call-interactively #'hyperdrive-open-previous-version))
       :active (hyperdrive-entry-previous hyperdrive-current-entry :cache-only t)
       :label (concat "Previous Version"
                      (pcase-exhaustive (hyperdrive-entry-previous hyperdrive-current-entry :cache-only t)
                        ('unknown (format " (?)"))
                        ('nil nil)
                        ((cl-struct hyperdrive-entry version)
                         (format " (%s)" version))))
       :help "Open previous version"]
      ["Next Version" (lambda ()
                        (interactive)
                        (call-interactively #'hyperdrive-open-next-version))
       :active (and (hyperdrive-entry-version hyperdrive-current-entry)
                    (hyperdrive-entry-next hyperdrive-current-entry))
       :label (concat "Next Version"
                      (when-let* ((entry hyperdrive-current-entry)
                                  (next-entry (hyperdrive-entry-next entry))
                                  ;; Don't add ": latest" if we're already at the latest version
                                  ((not (eq entry next-entry)))
                                  (display-version (if-let ((next-version (hyperdrive-entry-version next-entry)))
                                                       (number-to-string next-version)
                                                     "latest")))
                        (format " (%s)" display-version)))
       :help "Open next version"]
      ["Open Specific Version" (lambda ()
                                 (interactive)
                                 (call-interactively #'hyperdrive-open-at-version))
       :help "Open specific version"]
      ["Version History" (lambda ()
                           (interactive)
                           (call-interactively #'hyperdrive-history))
       :help "Open version history"]))
    "---"
    ("Bookmark"
     ["Bookmark Jump" hyperdrive-bookmark-jump
      :help "Jump to hyperdrive bookmark"]
     ["Bookmark List" hyperdrive-bookmark-list
      :help "List hyperdrive bookmarks"]
     ["Bookmark Set" bookmark-set
      :active hyperdrive-current-entry
      :help "Create a new hyperdrive bookmark"])
    "---"
    ["Customize" hyperdrive-customize
     :help "Customize hyperdrive options"]
    ["User Manual" hyperdrive-info-manual
     :help "Open hyperdrive.el info manual"]))

(easy-menu-define hyperdrive-easy-menu hyperdrive-mode-map
  "Menu with all Hyperdrive commands." hyperdrive-menu-bar-menu)

;;;###autoload
(define-minor-mode hyperdrive-menu-bar-mode "Show hyperdrive in \"Tools\" menu bar."
  :global t :group 'hyperdrive
  (if hyperdrive-menu-bar-mode
      (easy-menu-add-item menu-bar-tools-menu nil hyperdrive-menu-bar-menu
                          "Read Net News")
    (easy-menu-remove-item menu-bar-tools-menu nil "Hyperdrive")))

;;;;; Miscellaneous commands

;;;###autoload
(defun hyperdrive-customize ()
  "Customize Hyperdrive options."
  (interactive)
  (customize-group 'hyperdrive))

;;;###autoload
(defun hyperdrive-info-manual ()
  "Open hyperdrive.el info manual."
  (interactive)
  (info "(hyperdrive) Top"))

;;;;; Markdown link support

(defun hyperdrive--markdown-follow-link (url)
  "Follow URL.
For use in `markdown-follow-link-functions'."
  (pcase (url-type (url-generic-parse-url url))
    ((and `nil (guard (and hyperdrive-mode hyperdrive-current-entry)))
     (hyperdrive-open (hyperdrive--markdown-url-entry url))
     t)
    (_ nil)))

(defun hyperdrive--markdown-url-entry (url)
  "Return hyperdrive entry for URL in `markdown-mode' buffer.
Intended for relative (i.e. non-full) URLs."
  (pcase-let (((cl-struct url filename) (url-generic-parse-url url))
              ((cl-struct hyperdrive-entry hyperdrive path)
               hyperdrive-current-entry))
    ;; NOTE: Depending on the resolution of
    ;; <https://github.com/jrblevin/markdown-mode/issues/805>, we may
    ;; want to URL-decode paths.  For now, we won't.
    (hyperdrive-entry-create
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

(declare-function hyperdrive-menu "hyperdrive-menu-hyperdrive" nil t)

(with-eval-after-load 'embark
  (defvar-keymap hyperdrive-embark-hyperdrive-map
    :doc "Keymap for Embark actions on hyperdrives."
    :parent embark-general-map
    "h" #'hyperdrive-menu-hyperdrive
    "p" #'hyperdrive-set-petname
    "n" #'hyperdrive-set-nickname)

  (add-to-list 'embark-keymap-alist '(hyperdrive . hyperdrive-embark-hyperdrive-map)))

;;;; Footer

(provide 'hyperdrive)
;;; hyperdrive.el ends here
