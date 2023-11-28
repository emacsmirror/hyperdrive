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
(require 'h/ewoc)

;;;; Functions

;;;###autoload
(cl-defun hyperdrive-dir-handler (directory-entry &key then)
  "Show DIRECTORY-ENTRY.
If THEN, call it in the directory buffer with no arguments."
  ;; NOTE: ENTRY is not necessarily "filled" yet.
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path version) directory-entry)
               (url (he/url directory-entry))
               ((cl-struct plz-response headers body)
                ;; SOMEDAY: Consider updating plz to optionally not stringify the body.
                (h/api 'get url :as 'response :noquery t))
               (entry-names (json-read-from-string body))
               (entries (mapcar (lambda (entry-name)
                                  (he/create
                                   :hyperdrive hyperdrive
                                   :path (concat path entry-name)
                                   :version version))
                                entry-names))
               (parent-entry (h/parent directory-entry))
               (header
                (progn
                  ;; Fill metadata first to get the current nickname.
                  ;; TODO: Consider filling metadata earlier, outside
                  ;; of this function (e.g. so it will be available if
                  ;; the user loads a non-directory file directly).
                  (h/fill-metadata hyperdrive)
                  (h/dir-column-headers
                   (h//format-entry directory-entry))))
               (num-entries (length entries)) (num-filled 0)
	       ;; (debug-start-time (current-time))
               (metadata-queue) (ewoc) (prev-entry) (prev-point))
    (cl-labels ((goto-entry (entry ewoc)
                  (when-let ((node (h/ewoc-find-node ewoc entry
                                     :predicate #'he/equal-p)))
                    (goto-char (ewoc-location node))))
                (update-footer (num-filled num-of)
                  (when (zerop (mod num-filled 5))
                    (ewoc-set-hf ewoc header
                                 (propertize (format "Loading (%s/%s)..." num-filled num-of)
					     'face 'font-lock-comment-face)))))
      (setf directory-entry (h//fill directory-entry headers))
      (when parent-entry
        (setf (alist-get 'display-name (he/etc parent-entry))  "../")
        (push parent-entry entries))
      (with-current-buffer (h//get-buffer-create directory-entry)
        (with-silent-modifications
          (setf ewoc (or h/ewoc ; Bind this for lambdas.
                         (setf h/ewoc (ewoc-create #'h/dir-pp)))
                metadata-queue (make-plz-queue
				;; Experimentation seems to show that a
				;; queue size of about 20 performs best.
                                :limit h/queue-limit
                                :finally (lambda ()
                                           (with-current-buffer (ewoc-buffer ewoc)
                                             (with-silent-modifications
                                               ;; `with-silent-modifications' increases performance,
                                               ;; but we still need `set-buffer-modified-p' below.
                                               (ewoc-set-hf ewoc header "")
                                               (setf entries (h/sort-entries entries))
                                               (dolist (entry entries)
                                                 (ewoc-enter-last ewoc entry))
                                               (or (when prev-entry
                                                     (goto-entry prev-entry ewoc))
                                                   (goto-char prev-point)))
                                             (set-buffer-modified-p nil))
                                           ;; TODO: Remove this and the commented out `debug-start-time'
                                           ;; binding when we're done experimenting.
                                           ;; (message "Elapsed: %s"
                                           ;;          (float-time (time-subtract (current-time)
                                           ;;                                     debug-start-time)))
                                           ))
                prev-entry (when-let ((node (ewoc-locate h/ewoc)))
                             (ewoc-data node))
                prev-point (point))
          (ewoc-filter h/ewoc #'ignore)
          (update-footer num-filled num-entries)
          (dolist (entry entries)
            (h/fill entry :queue metadata-queue
              :then (lambda (&rest _)
                      (update-footer (cl-incf num-filled) num-entries))))
          (plz-run metadata-queue)
          (when then
            (funcall then)))))))

(defun h/dir-column-headers (prefix)
  "Return column headers as a string with PREFIX.
Columns are suffixed with up/down arrows according to
`hyperdrive-sort-entries'."
  (pcase-let* ((`(,sort-column . ,direction) h/directory-sort)
               ;; TODO: Use "↑" and "↓" glyphs, but make sure that the
               ;; column headers are aligned correctly.
               (arrow (propertize (if (eq direction :ascending) "^" "v")
                                  'face 'h/header-arrow))
               (headers))
    (pcase-dolist (`(,column . ,(map (:desc desc))) h/dir-sort-fields)
      (let* ((selected (eq column sort-column))
             ;; Put the arrow after desc, since the column is left-aligned.
             (left-aligned (eq column 'name))
             (format-str (pcase column
                           ('size "%6s")
                           ('mtime (format "%%%ds" h/timestamp-width))
                           ('name "%s")))
             (desc (concat (and selected (not left-aligned) (concat arrow " "))
                           (propertize desc 'face (if selected
                                                      'h/selected-column-header
                                                    'h/column-header))
                           ;; This extra space is necessary to prevent
                           ;; the `h/column-header' face from
                           ;; extended to the end of the window.
                           (and selected left-aligned (concat " " arrow)))))
        (push (propertize (format format-str desc)
                          'hyperdrive-dir-column column
                          'mouse-face 'highlight
                          'help-echo (format "Sort directory contents by %s" desc))
              headers)
        (unless (eq column 'name)
          ;; These gap spaces are necessary to prevent display mouse-face
          ;; from activating all contiguous strings simultaneously.
          (push "  " headers))))
    (apply #'concat prefix "\n" (nreverse headers))))


(defun h/dir-complete-sort ()
  "Return a value for `hyperdrive-directory-sort' selected with completion."
  (pcase-let* ((read-answer-short t)
               (choices (mapcar (lambda (field)
                                  (let ((desc (symbol-name (car field))))
                                    (list desc (aref desc 0) (format "sort by %s" desc))))
                                h/dir-sort-fields))
               (column (intern (read-answer "Sort by column: " choices))))
    (h/dir-toggle-sort-direction column h/directory-sort)))

(defun h/dir-toggle-sort-direction (column sort)
  "Return `hyperdrive-directory-sort' cons cell for COLUMN.
If SORT is already sorted using COLUMN, toggle direction.
Otherwise, set direction to \\+`:descending'."
  (pcase-let* ((`(,current-column . ,current-direction) sort)
               (direction (if (and (eq column current-column)
                                   (eq current-direction :ascending))
                              :descending
                            :ascending)))
    (cons column direction)))

(defun h/dir-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  (pcase-exhaustive thing
    ((pred he/p)
     (insert (h/dir--format-entry thing)))))

(defun h/dir--format-entry (entry)
  "Return ENTRY formatted as a string."
  (pcase-let* (((cl-struct hyperdrive-entry size mtime) entry)
               (size (when size
                       (file-size-human-readable size)))
               (directoryp (h//entry-directory-p entry))
               (face (if directoryp 'h/directory 'default))
               (timestamp (if mtime
                              (format-time-string h/timestamp-format mtime)
                            (propertize " " 'display '(space :width h/timestamp-width)))))
    (format "%6s  %s  %s"
            (propertize (or size "")
                        'face 'h/size)
            (propertize timestamp
                        'face 'h/timestamp)
            (propertize (or (alist-get 'display-name (he/etc entry))
                            (he/name entry))
                        'face face
                        'mouse-face 'highlight
                        'help-echo (format "Visit this %s in other window"
                                           (if directoryp "directory ""file"))))))

(defun h/dir--entry-at-point ()
  "Return entry at point.
With point below last entry, returns nil.
With point on header, returns directory entry."
  (let ((current-line (line-number-at-pos))
        (last-entry (ewoc-nth h/ewoc -1)))
    (cond ((or (not last-entry) (= 1 current-line))
           ;; Hyperdrive is empty or point is on header line
           h/current-entry)
          ((or (> current-line (line-number-at-pos (ewoc-location last-entry)))
               (= 2 current-line))
           ;; Point is below the last entry or on column headers
           nil)
          (t
           ;; Point on a file entry: return its entry.
           (ewoc-data (ewoc-locate h/ewoc))))))

;;;; Mode

(declare-function h/up "hyperdrive")
(declare-function h/delete "hyperdrive")
(declare-function h/download "hyperdrive")
;; `h/menu' is defined with `transient-define-prefix', which
;; `check-declare' doesn't recognize.
(declare-function h/menu "hyperdrive-menu" nil t)

(defvar-keymap h/dir-mode-map
  :parent h/ewoc-mode-map
  :doc "Local keymap for `hyperdrive-dir-mode' buffers."
  "RET" #'h/dir-find-file
  "o"   #'h/dir-find-file-other-window
  "v"   #'h/dir-view-file
  "j"   #'imenu
  "w"   #'h/dir-copy-url
  "d"   #'h/download
  "^"   #'h/up
  "D"   #'h/delete
  "H"   #'h/dir-history
  "s"   #'h/dir-sort
  "?"   #'h/menu
  "+"   #'h/create-directory-no-op
  "<mouse-2>" #'h/dir-follow-link
  "<follow-link>" 'mouse-face)

(define-derived-mode h/dir-mode h/ewoc-mode
  `("Hyperdrive-dir"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive directory buffers."
  :group 'hyperdrive
  :interactive nil
  (setq-local imenu-create-index-function #'h/dir--imenu-create-index-function
              imenu-auto-rescan t
              imenu-space-replacement " "))

;;;; Commands

(defun h/dir-follow-link (event)
  "Follow link at EVENT's position."
  (interactive "e")
  (if-let ((column (get-char-property (mouse-set-point event) 'hyperdrive-dir-column)))
      (h/dir-sort
       (h/dir-toggle-sort-direction
        column h/directory-sort))
    (call-interactively #'h/dir-find-file-other-window)))

(cl-defun h/dir-find-file
    (entry &key (display-buffer-action h/directory-display-buffer-action))
  "Visit hyperdrive ENTRY at point.
Interactively, visit file or directory at point in
`hyperdrive-dir' buffer.  DISPLAY-BUFFER-ACTION is passed to
`pop-to-buffer'."
  (declare (modes h/dir-mode))
  (interactive (list (or (h/dir--entry-at-point)
                         (h/user-error "No file/directory at point"))))
  (h/open entry
    :then (lambda ()
            (pop-to-buffer (current-buffer) display-buffer-action))))

(defun h/dir-find-file-other-window (entry)
  "Visit hyperdrive ENTRY at point in other window.
Interactively, visit file or directory at point in
`hyperdrive-dir' buffer."
  (declare (modes h/dir-mode))
  (interactive (list (or (h/dir--entry-at-point)
                         (h/user-error "No file/directory at point"))))
  (h/dir-find-file entry :display-buffer-action t))

(declare-function h/view-file "hyperdrive")
(defun h/dir-view-file (entry)
  "Open hyperdrive ENTRY at point in `view-mode'.
Interactively, opens file or directory at point in
`hyperdrive-dir' buffer."
  (declare (modes h/dir-mode))
  (interactive (list (or (h/dir--entry-at-point)
                         (h/user-error "No file/directory at point"))))
  (h/view-file entry))

(declare-function h/copy-url "hyperdrive")

(defun h/dir-copy-url (entry)
  "Copy URL of ENTRY into the kill ring."
  (declare (modes h/dir-mode))
  (interactive (list (or (h/dir--entry-at-point)
                         (h/user-error "No file/directory at point"))))
  (h/copy-url entry))

(declare-function h/history "hyperdrive-history")

(defun h/dir-history (entry)
  "Display version history for ENTRY at point."
  (interactive (list (or (h/dir--entry-at-point)
                         (h/user-error "No file/directory at point"))))
  (h/history entry))

(defun h/create-directory-no-op ()
  "Signal error that directory creation is not possible in hyperdrive."
  (interactive)
  (h/user-error "Cannot create empty directory; to create a new file, use `hyperdrive-find-file' or \\[hyperdrive-find-file]"))

(defun h/dir-sort (directory-sort)
  "Sort current `hyperdrive-dir' buffer by DIRECTORY-SORT.
DIRECTORY-SORT should be a valid value of
`hyperdrive-directory-sort'."
  (interactive (list (if current-prefix-arg
                         (h/dir-complete-sort)
                       (h/dir-toggle-sort-direction
                        (car h/directory-sort) h/directory-sort))))
  (setq-local h/directory-sort directory-sort)
  (with-silent-modifications
    (let ((entries (ewoc-collect h/ewoc #'he/p)))
      (ewoc-filter h/ewoc #'ignore)
      (dolist (entry (h/sort-entries entries))
        (ewoc-enter-last h/ewoc entry))
      (ewoc-set-hf h/ewoc
                   (h/dir-column-headers
                    (h//format-entry h/current-entry))
                   ""))))

;;;; Imenu support

(defun h/dir--imenu-create-index-function ()
  "Return Imenu index for the current `hyperdrive-dir' buffer.
For use as `imenu-create-index-function'."
  (cl-loop for node in (h/ewoc-collect-nodes h/ewoc #'identity)
           collect (let* ((location (goto-char (ewoc-location node)))
                          (entry (ewoc-data node))
                          (face (when (h//entry-directory-p entry)
                                  'h/directory)))
                     (cons (propertize (he/name entry)
                                       'face face)
                           location))))

;;;; Yank media support

(when (version<= "29.1" emacs-version)
  (defun h/dir--yank-media-image-handler (_type image)
    "Upload IMAGE to current buffer's hyperdrive directory.
Prompts for a filename before uploading.  For more information,
see Info node `(elisp)Yanking Media'."
    ;; TODO: Extend this to other media types?
    (cl-assert (and h/current-entry
                    (h//entry-directory-p h/current-entry)))
    (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) h/current-entry)
                 (entry (h/read-entry :hyperdrive (and (h/writablep hyperdrive)
                                                       hyperdrive)
                                      :predicate #'h/writablep
                                      :default-path path :latest-version t)))
      (h/api 'put (he/url entry)
        :body-type 'binary
        ;; TODO: Pass MIME type in a header? hyper-gateway detects it for us.
        :body image :as 'response
        :then (lambda (_res) (h/open entry))
        :else (lambda (plz-error)
                (h/message "Unable to yank media: %S" plz-error)))))

  (add-hook 'h/dir-mode-hook
            (lambda ()
              ;; Silence compiler warning about `yank-media-handler' not being
              ;; defined in earlier versions of Emacs.  (`with-suppressed-warnings'
              ;; doesn't allow suppressing this warning.)
              (with-no-warnings
                (yank-media-handler
                 "image/.*" #'h/dir--yank-media-image-handler)))))

(provide 'hyperdrive-dir)

;;;###autoload(register-definition-prefixes "hyperdrive-dir" '("hyperdrive-"))
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive-dir.el ends here
