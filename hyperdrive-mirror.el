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

;;;; Variables

(defvar-local hyperdrive-mirror-parent-entry nil
  "Parent entry for `hyperdrive-mirror-mode' buffer.")
(put 'hyperdrive-mirror-parent-entry 'permanent-local t)

(defvar-local hyperdrive-mirror-already-uploaded nil
  "Non-nil if files in `hyperdrive-mirror-mode' buffer have already been uploaded.")

;;;; Functions

(declare-function hyperdrive-open "hyperdrive")
(declare-function hyperdrive-upload-file "hyperdrive")
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

;;;; Commands

;; TODO: Don't overwrite a hyperdrive file with the same
;; contents. Should we keep a cache of uploaded files and mtimes?

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
         (parent-entry (hyperdrive-entry-create :hyperdrive hyperdrive :path target-dir :encode t))
         (files-and-urls
          ;; Structured according to `tabulated-list-entries'
          (mapcar (lambda (file)
                    (let ((url (hyperdrive-entry-url
                                (hyperdrive-entry-create
                                 :hyperdrive hyperdrive
                                 :path (expand-file-name (file-relative-name file source) target-dir)
                                 :encode t))))
                      ;; TODO: Wrap the first url in `substring-no-properties' to reduce memory usage?
                      (list url (vector file url))))
                  files)))
    (unless files
      (hyperdrive-user-error "No files selected for mirroring (double-check predicate)"))
    (if no-confirm
        (hyperdrive--mirror files-and-urls parent-entry)
      (pop-to-buffer (get-buffer-create "*hyperdrive-mirror*"))
      (hyperdrive-mirror-mode)
      (setq-local hyperdrive-mirror-already-uploaded nil)
      (setq-local hyperdrive-mirror-parent-entry parent-entry)
      (setf tabulated-list-entries files-and-urls)
      (tabulated-list-print))))

(defun hyperdrive-mirror-do-upload ()
  "Upload files in current \"*hyperdrive-mirror*\" buffer."
  (declare (modes hyperdrive-mirror-mode))
  (interactive)
  (if (and tabulated-list-entries hyperdrive-mirror-parent-entry)
      (when (or (not hyperdrive-mirror-already-uploaded) (yes-or-no-p "Already uploaded files.  Upload again?"))
        (hyperdrive--mirror tabulated-list-entries hyperdrive-mirror-parent-entry))
    (hyperdrive-user-error "Missing information about files to upload.  Are you in a \"*hyperdrive-mirror*\" buffer?")))

;;;; Mode

(defvar-keymap hyperdrive-mirror-mode-map
  :parent  tabulated-list-mode-map
  :doc "Local keymap for `hyperdrive-mirror-mode' buffers."
  "C-c C-c"   #'hyperdrive-mirror-do-upload)

(define-derived-mode hyperdrive-mirror-mode tabulated-list-mode
  "Hyperdrive-mirror"
  "Major mode for buffers for mirror local directories to a hyperdrive."
  :group 'hyperdrive
  :interactive nil
  ;; TODO: When possible, use vtable.el (currently only available in Emacs >=29)
  (setq tabulated-list-format [("From file" 60 t)
                               ("To URL" 60 t)])
  (tabulated-list-init-header))

;;;; Footer

(provide 'hyperdrive-mirror)
;;; hyperdrive-mirror.el ends here
