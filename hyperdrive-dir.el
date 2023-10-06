;;; hyperdrive-dir.el --- Hyperdrive directory frontend  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USHIN, Inc.

;; Author: Adam Porter <adam@alphapapa.net>

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
(require 'hyperdrive-ewoc)

;;;; Variables

(defvar imenu-auto-rescan)
(defvar imenu-space-replacement)

;;;; Functions

;;;###autoload
(cl-defun hyperdrive-dir-handler (directory-entry &key then)
  "Show DIRECTORY-ENTRY.
If THEN, call it in the directory buffer with no arguments after
the metadata has been loaded."
  ;; NOTE: ENTRY is not necessarily "filled" yet.
  ;; TODO: Set a timer and say "Opening URL..." if entry doesn't load
  ;; in a couple of seconds (same in hyperdrive-handler-default)
  ;; (like new with-delayed-message ?)
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path version) directory-entry)
               (url (hyperdrive-entry-url directory-entry))
               ((cl-struct plz-response headers body)
                ;; SOMEDAY: Consider updating plz to optionally not stringify the body.
                (hyperdrive-api 'get url :as 'response :noquery t))
               (entry-names (json-read-from-string body))
               (entries (mapcar (lambda (entry-name)
                                  (hyperdrive-entry-create
                                   :hyperdrive hyperdrive
                                   :path (concat (url-unhex-string path) entry-name)
                                   :version version))
                                entry-names))
               (parent-entry (hyperdrive-parent directory-entry))
               (header (hyperdrive-dir-column-headers (hyperdrive-entry-description directory-entry)))
               (num-entries (length entries)) (num-filled 0)
	       ;; (debug-start-time (current-time))
               (metadata-queue) (ewoc) (prev-entry) (prev-point))
    (cl-labels ((goto-entry (entry ewoc)
                  (when-let ((node (hyperdrive-ewoc-find-node ewoc entry
                                     :predicate #'hyperdrive-entry-equal-p)))
                    (goto-char (ewoc-location node))))
                (update-footer (num-filled num-of)
                  (when (zerop (mod num-filled 5))
                    (ewoc-set-hf ewoc header
                                 (propertize (format "Loading (%s/%s)..." num-filled num-of)
					     'face 'font-lock-comment-face)))))
      (hyperdrive-fill-metadata hyperdrive)
      (setf directory-entry (hyperdrive--fill directory-entry headers))
      (when parent-entry
        (setf (alist-get 'display-name (hyperdrive-entry-etc parent-entry))  "..")
        (push parent-entry entries))
      (with-current-buffer (hyperdrive--get-buffer-create directory-entry)
        (with-silent-modifications
          (setf ewoc (or hyperdrive-ewoc ; Bind this for lambdas.
                         (setf hyperdrive-ewoc (ewoc-create #'hyperdrive-dir-pp)))
                metadata-queue (make-plz-queue
				;; Experimentation seems to show that a
				;; queue size of about 20 performs best.
                                :limit hyperdrive-queue-limit
                                :finally (lambda ()
                                           (with-current-buffer (ewoc-buffer ewoc)
                                             (with-silent-modifications
                                               ;; `with-silent-modifications' increases performance,
                                               ;; but we still need `set-buffer-modified-p' below.
                                               (ewoc-set-hf ewoc header "")
                                               (setf entries (hyperdrive-sort-entries entries))
                                               (dolist (entry entries)
                                                 (ewoc-enter-last ewoc entry))
                                               (or (when prev-entry
                                                     (goto-entry prev-entry ewoc))
                                                   (goto-char prev-point)))
                                             (set-buffer-modified-p nil)
                                             (when then
                                               (funcall then)))
                                           ;; TODO: Remove this and the commented out `debug-start-time'
                                           ;; binding when we're done experimenting.
                                           ;; (message "Elapsed: %s"
                                           ;;          (float-time (time-subtract (current-time)
                                           ;;                                     debug-start-time)))
                                           ))
                prev-entry (when-let ((node (ewoc-locate hyperdrive-ewoc)))
                             (ewoc-data node))
                prev-point (point))
          (ewoc-filter hyperdrive-ewoc #'ignore)
          (update-footer num-filled num-entries)
          (dolist (entry entries)
            (hyperdrive-fill entry :queue metadata-queue
              :then (lambda (&rest _)
                      (update-footer (cl-incf num-filled) num-entries))))
          (plz-run metadata-queue)
          (display-buffer (current-buffer) hyperdrive-directory-display-buffer-action))))))

(defun hyperdrive-dir-column-headers (prefix)
  "Return column headers as a string with PREFIX.
Columns are suffixed with up/down arrows according to
`hyperdrive-sort-entries'."
  (pcase-let* ((`(,sort-column . ,direction) hyperdrive-directory-sort)
               ;; TODO: Use "↑" and "↓" glyphs, but make sure that the
               ;; column headers are aligned correctly.
               (arrow (propertize (if (eq direction :ascending) "^" "v")
                                  'face 'hyperdrive-header-arrow))
               (headers))
    (pcase-dolist (`(,column . ,(map (:desc desc))) hyperdrive-dir-sort-fields)
      (let* ((selected (eq column sort-column))
             ;; Put the arrow after desc, since the column is left-aligned.
             (left-aligned (eq column 'name))
             (format-str (pcase column
                           ('size "%6s")
                           ('mtime (format "%%%ds" hyperdrive-timestamp-width))
                           ('name (format "%%-%ds" (- (window-width) 6 2 hyperdrive-timestamp-width 2)))))
             (desc (concat (and selected (not left-aligned) arrow)
                           (and (not left-aligned) " ")
                           (propertize desc 'face (if selected
                                                      'hyperdrive-selected-column-header
                                                    'hyperdrive-column-header))
                           ;; This extra space is necessary to prevent
                           ;; the `hyperdrive-column-header' face from
                           ;; extended to the end of the window.
                           (and left-aligned " ")
                           (and selected left-aligned arrow))))
        (push (propertize (format format-str desc)
                          'keymap
                          (define-keymap
                            "<mouse-1>" (lambda (&optional _e)
                                          (interactive "e")
                                          (hyperdrive-dir-sort
                                           (hyperdrive-dir-toggle-sort-direction
                                            column hyperdrive-directory-sort))))
                          'mouse-face 'highlight)
              headers)
        (unless (eq column 'name)
          ;; These gap spaces are necessary to prevent display mouse-face
          ;; from activating all contiguous strings simultaneously.
          (push "  " headers))))
    (apply #'concat prefix "\n" (nreverse headers))))


(defun hyperdrive-dir-complete-sort ()
  "Return a value for `hyperdrive-directory-sort' selected with completion."
  (pcase-let* ((read-answer-short t)
               (choices (mapcar (lambda (field)
                                  (let ((desc (symbol-name (car field))))
                                    (list desc (aref desc 0) (format "sort by %s" desc))))
                                hyperdrive-dir-sort-fields))
               (column (intern (read-answer "Sort by column: " choices))))
    (hyperdrive-dir-toggle-sort-direction column hyperdrive-directory-sort)))

(defun hyperdrive-dir-toggle-sort-direction (column sort)
  "Return `hyperdrive-directory-sort' cons cell for COLUMN.
If SORT is already sorted using COLUMN, toggle direction.
Otherwise, set direction to \\+`:descending'."
  (pcase-let* ((`(,current-column . ,current-direction) sort)
               (direction (if (and (eq column current-column)
                                   (eq current-direction :ascending))
                              :descending
                            :ascending)))
    (cons column direction)))

(defun hyperdrive-dir-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  (pcase-exhaustive thing
    ((pred hyperdrive-entry-p)
     (insert (hyperdrive-dir--format-entry thing)))))

(defun hyperdrive-dir--format-entry (entry)
  "Return ENTRY formatted as a string."
  (pcase-let* (((cl-struct hyperdrive-entry size mtime) entry)
               (size (when size
                       (file-size-human-readable size)))
               (face (if (hyperdrive--entry-directory-p entry)
                         'hyperdrive-directory
                       'default))
               (timestamp (if mtime
                              (format-time-string hyperdrive-timestamp-format mtime)
                            (propertize " " 'display '(space :width hyperdrive-timestamp-width)))))
    (format "%6s  %s  %s"
            (propertize (or size "")
                        'face 'hyperdrive-size)
            (propertize timestamp
                        'face 'hyperdrive-timestamp)
            (propertize (or (alist-get 'display-name (hyperdrive-entry-etc entry))
                            (hyperdrive-entry-name entry))
                        'face face))))

(defun hyperdrive-dir--entry-at-point ()
  "Return entry at point.
With point below last entry, returns nil.
With point on header, returns directory entry."
  (let ((current-line (line-number-at-pos))
        (last-line (line-number-at-pos (ewoc-location (ewoc-nth hyperdrive-ewoc -1))))
        (entry-at-point (ewoc-data (ewoc-locate hyperdrive-ewoc))))
    (cond ((= 1 current-line)
           ;; Point on header: return directory's entry.
           hyperdrive-current-entry)
          ((or (> current-line last-line) (= 2 current-line))
           ;; Point is below the last entry or on column headers: signal error.
           (hyperdrive-user-error "No file on this line"))
          (t
           ;; Point on a file entry: return its entry.
           entry-at-point))))

;;;; Mode

(declare-function hyperdrive-find-file "hyperdrive")
(declare-function hyperdrive-up "hyperdrive")
(declare-function hyperdrive-download "hyperdrive")
(declare-function hyperdrive-describe-hyperdrive "hyperdrive-describe")
;; `hyperdrive-menu' is defined with `transient-define-prefix', which
;; `check-declare' doesn't recognize.
(declare-function hyperdrive-menu "hyperdrive-menu" nil t)

(defvar-keymap hyperdrive-dir-mode-map
  :parent hyperdrive-ewoc-mode-map
  :doc "Local keymap for `hyperdrive-dir-mode' buffers."
  "RET" #'hyperdrive-dir-find-file
  "f"   #'hyperdrive-find-file ;; Alternatively, define new function which fills in name of entry at point.
  "v"   #'hyperdrive-dir-view-file
  "j"   #'imenu
  "w"   #'hyperdrive-dir-copy-url
  "d"   #'hyperdrive-download
  "^"   #'hyperdrive-up
  ;; TODO(doc): hyperdrive-dir-delete replaced by hyperdrive-delete
  "D"   #'hyperdrive-delete
  "H"   #'hyperdrive-dir-history
  "o"   #'hyperdrive-dir-sort
  "?"   #'hyperdrive-menu
  "+"   #'hyperdrive-create-directory-no-op)

(define-derived-mode hyperdrive-dir-mode hyperdrive-ewoc-mode
  `("Hyperdrive-dir"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive directory buffers."
  :group 'hyperdrive
  :interactive nil
  (setq-local imenu-create-index-function #'hyperdrive-dir--imenu-create-index-function
              imenu-auto-rescan t
              imenu-space-replacement " "))

;;;; Commands

(defun hyperdrive-dir-find-file (entry)
  "Visit hyperdrive ENTRY at point.
Interactively, visit file or directory at point in
`hyperdrive-dir' buffer."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (hyperdrive-open entry))

(declare-function hyperdrive-view-file "hyperdrive")
(defun hyperdrive-dir-view-file (entry)
  "Open hyperdrive ENTRY at point in `view-mode'.
Interactively, opens file or directory at point in
`hyperdrive-dir' buffer."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (hyperdrive-view-file entry))

(declare-function hyperdrive-copy-url "hyperdrive")

(defun hyperdrive-dir-copy-url (entry)
  "Copy URL of ENTRY into the kill ring."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (hyperdrive-copy-url entry))

(declare-function hyperdrive-history "hyperdrive-history")

(defun hyperdrive-dir-history (entry)
  "Display version history for ENTRY at point."
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (hyperdrive-history entry))

(defun hyperdrive-create-directory-no-op ()
  "Signal error that directory creation is not possible in hyperdrive."
  (interactive)
  (hyperdrive-user-error
   (substitute-command-keys "Cannot create empty directory; to create a new file, use `hyperdrive-find-file' or \\[hyperdrive-find-file]")))

(defun hyperdrive-dir-sort (directory-sort)
  "Sort current `hyperdrive-dir' buffer by DIRECTORY-SORT.
DIRECTORY-SORT should be a valid value of
`hyperdrive-directory-sort'."
  (interactive (list (hyperdrive-dir-complete-sort)))
  (setq-local hyperdrive-directory-sort directory-sort)
  (with-silent-modifications
    (let ((entries (ewoc-collect hyperdrive-ewoc #'hyperdrive-entry-p)))
      (ewoc-filter hyperdrive-ewoc #'ignore)
      (dolist (entry (hyperdrive-sort-entries entries))
        (ewoc-enter-last hyperdrive-ewoc entry))
      (ewoc-set-hf hyperdrive-ewoc
                   (hyperdrive-dir-column-headers (hyperdrive-entry-description hyperdrive-current-entry))
                   ""))))

;;;; Imenu support

(defun hyperdrive-dir--imenu-create-index-function ()
  "Return Imenu index for the current `hyperdrive-dir' buffer.
For use as `imenu-create-index-function'."
  (cl-loop for node in (hyperdrive-ewoc-collect-nodes hyperdrive-ewoc #'identity)
           collect (let* ((location (goto-char (ewoc-location node)))
                          (entry (ewoc-data node))
                          (face (when (hyperdrive--entry-directory-p entry)
                                  'hyperdrive-directory)))
                     (cons (propertize (hyperdrive-entry-name entry)
                                       'face face)
                           location))))

;;;; Yank media support

(when (version<= "29.1" emacs-version)
  (defun hyperdrive-dir--yank-media-image-handler (_type image)
    "Upload IMAGE to current buffer's hyperdrive directory.
Prompts for a filename before uploading.  For more information,
see Info node `(elisp)Yanking Media'."
    ;; TODO: Extend this to other media types?
    (cl-assert (and hyperdrive-current-entry
                    (hyperdrive--entry-directory-p hyperdrive-current-entry)))
    (let ((entry (hyperdrive-read-entry :predicate #'hyperdrive-writablep
                                        :default-path (hyperdrive-entry-path hyperdrive-current-entry)
                                        :allow-version-p nil)))
      (hyperdrive-api 'put (hyperdrive-entry-url entry)
        :body-type 'binary
        ;; TODO: Pass MIME type in a header? hyper-gateway detects it for us.
        :body image :as 'response
        :then (lambda (_res) (hyperdrive-open entry))
        :else (lambda (plz-error)
                (hyperdrive-message "Unable to yank media: %S" plz-error)))))

  (add-hook 'hyperdrive-dir-mode-hook
            (lambda ()
              ;; Silence compiler warning about `yank-media-handler' not being
              ;; defined in earlier versions of Emacs.  (`with-suppressed-warnings'
              ;; doesn't allow suppressing this warning.)
              (with-no-warnings
                (yank-media-handler
                 "image/.*" #'hyperdrive-dir--yank-media-image-handler)))))

(provide 'hyperdrive-dir)
;;; hyperdrive-dir.el ends here
