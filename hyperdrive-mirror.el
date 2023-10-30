;;; hyperdrive-mirror.el --- Display information about a hyperdrive  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;;; Requirements

(require 'cl-lib)

(require 'hyperdrive-lib)

(require 'taxy-magit-section)

;;;; Variables

;; TODO: Consolidate these two local variables into one?
(defvar-local hyperdrive-mirror-parent-entry nil
  "Parent entry for `hyperdrive-mirror-mode' buffer.")
(put 'hyperdrive-mirror-parent-entry 'permanent-local t)

(defvar-local hyperdrive-mirror-files-and-urls nil
  "List of lists like (FILE STATUS URL) for `hyperdrive-mirror-mode'.")

(defvar-local hyperdrive-mirror-query nil
  "List of arguments passed to `hyperdrive-mirror', excluding \\+`no-confirm'.")

(defvar-local hyperdrive-mirror-visibility-cache nil)

;;;; Keys

;; These are the "keys" used to group items with Taxy.

(eval-and-compile
  (taxy-define-key-definer hyperdrive-mirror-define-key
    hyperdrive-mirror-keys "hyperdrive-mirror-key" "Grouping keys."))

(hyperdrive-mirror-define-key status (&key name status)
  (pcase-let ((`(,_file ,item-status ,_url) item))
    (if status
        (when (equal status item-status)
          (or name status))
      item-status)))

(defvar hyperdrive-mirror-default-keys
  '(status)
  "Default keys.")

;;;; Columns

;; These forms define the columns used to display items with `taxy-magit-section'.

(eval-and-compile
  (taxy-magit-section-define-column-definer "hyperdrive-mirror"))

(hyperdrive-mirror-define-column "File" ()
  (pcase-let* ((`(,file ,_status ,_url) item))
    file))

(hyperdrive-mirror-define-column "Status" ()
  (pcase-let* ((`(,_file ,status ,_url) item))
    status))

(hyperdrive-mirror-define-column "URL" ()
  (pcase-let* ((`(,_file ,_status ,url) item))
    url))

(unless hyperdrive-mirror-columns
  (setq-default hyperdrive-mirror-columns
                (get 'hyperdrive-mirror-columns 'standard-value)))

;;;; Functions

(declare-function hyperdrive-upload-file "hyperdrive")
(defun hyperdrive--mirror (files-and-urls parent-entry)
  "Upload each file to its corresponding URL in FILES-AND-URLs.
FILES-AND-URLS is structured like `hyperdrive-mirror-files-and-urls'.
After uploading files, open PARENT-ENTRY."
  (let* ((count 0)
         (upload-files-and-urls (cl-remove-if-not (pcase-lambda (`(,_file ,status ,_url))
                                                    (string-match-p (rx (or "not in" "newer than")) status))
                                                  files-and-urls))
         (progress-reporter
          (make-progress-reporter (format "Uploading %s files: " (length upload-files-and-urls)) 0 (length upload-files-and-urls)))
         (queue (make-plz-queue
                 :limit hyperdrive-queue-limit
                 :finally (lambda ()
                            (progress-reporter-done progress-reporter)
                            (hyperdrive-open parent-entry)
                            (with-current-buffer (get-buffer-create "*hyperdrive-mirror*")
                              (revert-buffer nil t))))))
    (unless upload-files-and-urls
      (hyperdrive-user-error "No new/newer files to upload"))
    (pcase-dolist (`(,file ,_status ,url) upload-files-and-urls)
      (hyperdrive-upload-file file (hyperdrive-url-entry url)
        :queue queue
        ;; TODO: Error handling (e.g. in case one or more files fails to upload).
        :then (lambda (_)
                (progress-reporter-update progress-reporter (cl-incf count)))))))

(defun hyperdrive-mirror-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `hyperdrive-mirror-mode' buffer.
Runs `hyperdrive-mirror' again with the same query."
  (apply #'hyperdrive-mirror hyperdrive-mirror-query))

;;;; Commands

;; TODO: Rewrite `hyperdrive-mirror' as a Transient.

;;;###autoload
(cl-defun hyperdrive-mirror
    (source hyperdrive &key target-dir (predicate #'always) no-confirm)
  "Mirror SOURCE to TARGET-DIR in HYPERDRIVE.

Only mirror paths within SOURCE for which PREDICATE returns
non-nil.  PREDICATE may be a function, which receives the expanded
filename path as its argument, or a regular expression, which is
tested against each expanded filename path.  SOURCE is a directory
name.

When TARGET-DIR is nil, SOURCE is mirrored into the
hyperdrive's root directory \"/\".

Opens the \"*hyperdrive-mirror*\" buffer with the list of files to
be uploaded and the URL at which each file will be published.  See
`hyperdrive-mirror-mode'.

When NO-CONFIRM is non-nil, upload without prompting.

Interactively, with one universal prefix argument
\\[universal-argument], prompt for predicate, otherwise mirror
all files. With two universal prefix arguments
\\[universal-argument] \\[universal-argument], prompt for
predicate and set NO-CONFIRM to t."
  (interactive
   (let ((source (read-directory-name "Mirror directory: " nil nil t))
         (hyperdrive (hyperdrive-complete-hyperdrive :predicate #'hyperdrive-writablep
                                                     :force-prompt t)))
     (list source hyperdrive
           ;; TODO: Get path from any visible hyperdrive-dir buffer and
           ;; auto-fill (or add as "future history") in target-dir prompt.
           :target-dir (hyperdrive-read-path :hyperdrive hyperdrive :prompt "Target directory in «%s»" :default "/")
           :no-confirm (equal '(16) current-prefix-arg)
           :predicate (if current-prefix-arg
                          (hyperdrive-mirror-read-predicate)
                        #'always))))
  (cl-callf expand-file-name source)
  (setf target-dir (hyperdrive--format-path target-dir :directoryp t))
  (when (stringp predicate)
    (let ((regexp predicate))
      (setf predicate (lambda (filename)
                        (string-match-p regexp filename)))))
  (let* ((files (cl-remove-if-not predicate (directory-files-recursively source ".")))
         (parent-entry (hyperdrive-entry-create :hyperdrive hyperdrive :path target-dir))
         (buffer (unless no-confirm
                   (get-buffer-create "*hyperdrive-mirror*")))
         (num-filled 0)
         (num-of (length files))
         metadata-queue files-and-urls)
    (unless files
      (hyperdrive-user-error "No files selected for mirroring (double-check predicate)"))
    (if no-confirm
        (hyperdrive--mirror files-and-urls parent-entry)
      (with-current-buffer buffer
        (with-silent-modifications
          (cl-labels ((update-progress (num-filled num-of)
                        (when (zerop (mod num-filled 5))
                          (with-current-buffer buffer
                            (with-silent-modifications
                              (erase-buffer)
                              (insert (propertize (format "Comparing files (%s/%s)..." num-filled num-of)
                                                  'face 'font-lock-comment-face)))))))
            (hyperdrive-mirror-mode)
            (setq-local hyperdrive-mirror-query
                        `(,source ,hyperdrive :target-dir ,target-dir :predicate ,predicate)
                        hyperdrive-mirror-parent-entry parent-entry)
            ;; TODO: Add command to clear plz queue.
            (setf metadata-queue
                  (make-plz-queue
                   :limit hyperdrive-queue-limit
                   :finally (lambda ()
                              (hyperdrive-mirror--metadata-finally
                               buffer
                               (sort files-and-urls
                                     (pcase-lambda (`(,_ ,a-file ,_) `(,_ ,b-file ,_))
                                       (string< a-file b-file)))))))
            (dolist (file files)
              (let ((entry (hyperdrive-entry-create
                            :hyperdrive hyperdrive
                            :path (expand-file-name (file-relative-name file source) target-dir))))
                (hyperdrive-fill entry :queue metadata-queue
                  :then (lambda (entry)
                          (let* ((drive-mtime (hyperdrive-entry-mtime entry))
                                 (local-mtime (file-attribute-modification-time (file-attributes file)))
                                 (status (cond
                                          ((time-equal-p drive-mtime local-mtime)
                                           (propertize "same as" 'face 'hyperdrive-mirror-same))
                                          ((time-less-p drive-mtime local-mtime)
                                           (propertize "newer than" 'face 'hyperdrive-mirror-newer))
                                          (t
                                           (propertize "older than" 'face 'hyperdrive-mirror-older))))
                                 (url (hyperdrive-entry-url entry)))
                            (push (list file status url) files-and-urls)
                            (update-progress (cl-incf num-filled) num-of)))
                  :else (lambda (plz-error)
                          (let ((status-code (plz-response-status (plz-error-response plz-error))))
                            (pcase status-code
                              (404 ;; Entry doesn't exist: Set `status' to `new'.
                               ;; TODO: Consider moving `hyperdrive-update-nonexistent-version-range' call...
                               (hyperdrive-update-nonexistent-version-range entry)
                               (let ((status (propertize "not in" 'face 'hyperdrive-mirror-new))
                                     (url (hyperdrive-entry-url entry)))
                                 (push (list file status url) files-and-urls)
                                 (update-progress (cl-incf num-filled) num-of)))
                              (_
                               (hyperdrive-error "Unable to get metadata for URL \"%s\": %S"
                                                 (hyperdrive-entry-url entry) plz-error))))))))
            (pop-to-buffer (current-buffer))))))))

(defun hyperdrive-mirror--metadata-finally (buffer files-and-urls)
  "Insert FILES-AND-URLS into BUFFER.
Callback for queue finalizer in `hyperdrive-mirror'."
  (with-current-buffer buffer
    (with-silent-modifications
      (let ((pos (point))
            (section-ident (when (magit-current-section)
                             (magit-section-ident (magit-current-section))))
            (window-start 0) (window-point 0))
        (setq-local hyperdrive-mirror-files-and-urls files-and-urls)
        (when-let ((window (get-buffer-window (current-buffer))))
          (setf window-point (window-point window)
                window-start (window-start window)))
        (when hyperdrive-mirror-visibility-cache
          (setf magit-section-visibility-cache hyperdrive-mirror-visibility-cache))
        (add-hook 'kill-buffer-hook #'hyperdrive-mirror--cache-visibility nil 'local)
        (delete-all-overlays)
        (erase-buffer)
        (hyperdrive-mirror--insert-taxy :items files-and-urls)
        (if-let ((section-ident)
                 (section (magit-get-section section-ident)))
            (goto-char (oref section start))
          (goto-char pos))
        (when-let ((window (get-buffer-window (current-buffer))))
          (set-window-start window window-start)
          (set-window-point window window-point))))
    (set-buffer-modified-p nil)))

(cl-defun hyperdrive-mirror--insert-taxy
    (&key items (keys hyperdrive-mirror-default-keys))
  "Insert and return a `taxy' for `hyperdrive-mirror', optionally having ITEMS.
KEYS should be a list of grouping keys, as in
`hyperdrive-mirror-default-keys'."
  (let (format-table column-sizes)
    (cl-labels ((format-item (item) (gethash item format-table))
                (make-fn (&rest args)
                  (apply #'make-taxy-magit-section
                         :make #'make-fn
                         :format-fn #'format-item
                         ;; FIXME: Make indent an option again.
                         :level-indent 2
                         ;; :visibility-fn #'visible-p
                         ;; :heading-indent 2
                         :item-indent 0
                         ;; :heading-face-fn #'heading-face
                         args)))
      (let* ((taxy-magit-section-insert-indent-items nil)
             ;; (taxy-magit-section-item-indent 0)
             ;; (taxy-magit-section-level-indent 0)
             (taxy
              (thread-last
                (make-fn :name "Hyperdrive mirror"
                         :take (taxy-make-take-function keys hyperdrive-mirror-keys))
                (taxy-fill items)))
             (format-cons
              (taxy-magit-section-format-items
               hyperdrive-mirror-columns hyperdrive-mirror-column-formatters
               taxy))
             (inhibit-read-only t))
        (setf format-table (car format-cons)
              column-sizes (cdr format-cons)
              header-line-format (taxy-magit-section-format-header
                                  column-sizes hyperdrive-mirror-column-formatters))
        ;; Before this point, no changes have been made to the buffer's contents.
        (save-excursion
          (taxy-magit-section-insert taxy :items 'first :initial-depth 0))
        taxy))))

(defun hyperdrive-mirror-read-predicate ()
  "Read a function for filtering source files for mirroring."
  (let* ((readers
          '(("Mirror all files" .
             (lambda () #'always))
            ("`rx' form" .
             (lambda () (eval (read--expression "`rx' form: " "(rx )"))))
            ("Regexp string" .
             (lambda () (read-regexp "Regular expression: ")))
            ("Lambda function" .
             (lambda () (read--expression "Lambda: " "(lambda (filename) )")))
            ("Named function"   .
             (lambda () (completing-read "Named function: " obarray #'functionp t)))))
         (reader (completing-read "Predicate type: " readers)))
    (funcall (alist-get reader readers nil nil #'equal))))

(defun hyperdrive-mirror-do-upload ()
  "Upload files in current \"*hyperdrive-mirror*\" buffer."
  (declare (modes hyperdrive-mirror-mode))
  (interactive)
  ;; FIXME: Debounce this (e.g. if the user accidentally calls this
  ;; command twice in a mirror buffer, it would start another queue to
  ;; upload the same files, which would unnecessarily increment the
  ;; hyperdrive version by potentially a lot).
  (if (and hyperdrive-mirror-files-and-urls hyperdrive-mirror-parent-entry)
      (hyperdrive--mirror hyperdrive-mirror-files-and-urls hyperdrive-mirror-parent-entry)
    (hyperdrive-user-error "Missing information about files to upload.  Are you in a \"*hyperdrive-mirror*\" buffer?")))

(defun hyperdrive-mirror--cache-visibility ()
  "Save visibility cache.
Sets `hyperdrive-mirror-visibility-cache' to the value of
`magit-section-visibility-cache'.  To be called in
`kill-buffer-hook' in `hyperdrive-mirror' buffers."
  (ignore-errors
    (when magit-section-visibility-cache
      (setf hyperdrive-mirror-visibility-cache magit-section-visibility-cache))))

;;;; Mode

(defvar-keymap hyperdrive-mirror-mode-map
  :parent magit-section-mode-map
  :doc "Local keymap for `hyperdrive-mirror-mode' buffers."
  "C-c C-c"   #'hyperdrive-mirror-do-upload)

(define-derived-mode hyperdrive-mirror-mode magit-section-mode
  "Hyperdrive-mirror"
  "Major mode for buffers for mirror local directories to a hyperdrive."
  :group 'hyperdrive
  :interactive nil
  ;; TODO: When possible, use vtable.el (currently only available in Emacs >=29) (or maybe taxy-magit-section)
  (setq revert-buffer-function #'hyperdrive-mirror-revert-buffer))

;;;; Footer

(provide 'hyperdrive-mirror)
;;; hyperdrive-mirror.el ends here
