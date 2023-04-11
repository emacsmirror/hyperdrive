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
(require 'dired)  ; For faces.
(require 'ewoc)

(require 'hyperdrive-lib)

;;;; Variables

(defvar-local hyperdrive-dir-ewoc nil
  "EWOC for current hyperdrive buffer.")

(defvar-local hyperdrive-entries nil
  "Entries in current hyperdrive buffer.")

(defvar hyperdrive-current-entry)
(defvar hyperdrive-timestamp-format)
(defvar hyperdrive-default-host-format)
(defvar hyperdrive-download-directory)

;;;; Faces

(defgroup hyperdrive-faces nil
  "Faces shown in directory listings."
  :group 'hyperdrive)

(defface hyperdrive-header
  '((t (:inherit dired-header)))
  "Directory path.")

(defface hyperdrive-directory
  '((t (:inherit dired-directory)))
  "Subdirectories.")

(defface hyperdrive-size
  '((t (:inherit font-lock-doc-face)))
  "Size of entries.")

(defface hyperdrive-timestamp
  '((t (:inherit default)))
  "Entry timestamp.")

;;;; Functions

(defun hyperdrive-dir-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  (pcase-exhaustive thing
    ((pred hyperdrive-entry-p)
     (insert (hyperdrive-dir--format-entry thing)))))

(defun hyperdrive-dir--format-entry (entry)
  "Return ENTRY formatted as a string."
  (pcase-let* (((cl-struct hyperdrive-entry size modified) entry)
               (size (when size
                       (file-size-human-readable size)))
               (face (if (hyperdrive--entry-directory-p entry)
                         'hyperdrive-directory
                       'default))
               (timestamp (if modified
                              (format-time-string hyperdrive-timestamp-format modified)
                            (format hyperdrive-timestamp-format-string " "))))
    (format "%6s  %s  %s"
            (propertize (or size "")
                        'face 'hyperdrive-size)
            (propertize timestamp
                        'face 'hyperdrive-timestamp)
            (propertize (or (alist-get 'display-name (hyperdrive-entry-etc entry))
                            (hyperdrive-entry-name entry))
                        'face face))))

;;;; Mode

(defvar-keymap hyperdrive-dir-mode-map
  :parent  special-mode-map
  :doc "Local keymap for `hyperdrive-dir-mode' buffers."
  "RET" #'hyperdrive-dir-find-file
  "^"   #'hyperdrive-dir-up
  "w"   #'hyperdrive-dir-copy-url-as-kill
  "d"   #'hyperdrive-dir-download-file
  "n"   #'hyperdrive-dir-next
  "p"   #'hyperdrive-dir-previous
  "D"   #'hyperdrive-dir-delete)

(declare-function hyperdrive-revert-buffer "hyperdrive")
(declare-function hyperdrive-bookmark-make-record "hyperdrive")

(define-derived-mode hyperdrive-dir-mode special-mode
  `("Hyperdrive-dir"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive directory buffers."
  :interactive nil
  (let ((inhibit-read-only t))
    (erase-buffer))
  (hl-line-mode)
  (setq-local revert-buffer-function #'hyperdrive-revert-buffer
              bookmark-make-record-function #'hyperdrive-bookmark-make-record)
  (setf hyperdrive-dir-ewoc (ewoc-create #'hyperdrive-dir-pp)
        ;; TODO(alphapapa): Imenu support.
        ;; imenu-create-index-function #'ement-room--imenu-create-index-function
        ))

;;;; Commands

;; TODO: Implement sorting by size, type, etc.

(declare-function hyperdrive-open "hyperdrive")

(defun hyperdrive-dir-find-file (entry)
  "Visit hyperdrive ENTRY.
Interactively, visit file or directory at point in
`hyperdrive-dir' buffer."
  (interactive (list (ewoc-data (ewoc-locate hyperdrive-dir-ewoc))))
  (hyperdrive-open entry))

(defun hyperdrive-dir-up ()
  "Go up to parent directory."
  (interactive)
  (if-let ((parent (hyperdrive-parent hyperdrive-current-entry)))
      (hyperdrive-open parent)
    (user-error "At root directory")))

(defun hyperdrive-dir--entry-at-point ()
  "Return entry at point.
With point on header, return directory entry."
  (cond ((= 1 (line-number-at-pos))
         ;; Point on header: copy directory's entry.
         hyperdrive-current-entry)
        ((> (line-number-at-pos)
            (line-number-at-pos (ewoc-location (ewoc-nth hyperdrive-dir-ewoc -1))))
         ;; Point is below the last entry: return nil.
         nil)
        (t
         ;; Point on a file entry: copy its entry.
         (ewoc-data (ewoc-locate hyperdrive-dir-ewoc)))))

(declare-function hyperdrive-copy-url "hyperdrive")

(defun hyperdrive-dir-copy-url-as-kill (entry)
  "Copy URL of ENTRY into the kill ring."
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (hyperdrive-copy-url entry))

(declare-function hyperdrive-download-entry "hyperdrive")

(defun hyperdrive-dir-download-file (entry filename)
  "Download ENTRY at point to FILENAME on disk."
  (interactive
   (pcase-let* ((entry (hyperdrive-dir--entry-at-point))
                ((cl-struct hyperdrive-entry name) entry)
                (read-filename (read-string "Filename: " (expand-file-name name hyperdrive-download-directory))))
     (list entry read-filename)))
  (hyperdrive-download-entry entry filename))

(defun hyperdrive-dir-delete (entry)
  "Delete ENTRY."
  (interactive (list (ewoc-data (ewoc-locate hyperdrive-dir-ewoc))))
  (pcase-let (((cl-struct hyperdrive-entry name) entry)
              (buffer (current-buffer)))
    (when (yes-or-no-p (format "Delete %S? " name))
      (hyperdrive-delete entry
        :then (lambda (_)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (revert-buffer)))
                (hyperdrive-message "Deleted: %S (Deleted files can be accessed from prior versions of the hyperdrive.)" name))
        :else (lambda (plz-error)
                (hyperdrive-message "Unable to delete %S: %S" name plz-error))))))

(cl-defun hyperdrive-dir-next (&optional (n 1))
  "Move forward N entries."
  (interactive "p")
  (cond ((= 1 (line-number-at-pos))
         ;; Point on header: move into first entry.
         (forward-line 1))
        (t
         ;; Point is elsewhere: move to next entry (`ewoc-next' won't
         ;; move past the last entry).
         (hyperdrive-dir-move n))))

(cl-defun hyperdrive-dir-previous (&optional (n 1))
  "Move backward N entries."
  (interactive "p")
  (cond ((= 2 (line-number-at-pos))
         ;; Point on first entry: move into header.
         (forward-line -1))
        ((> (line-number-at-pos)
            (line-number-at-pos (ewoc-location (ewoc-nth hyperdrive-dir-ewoc -1))))
         ;; Point is past last entry: move to last entry.
         (goto-char (ewoc-location (ewoc-nth hyperdrive-dir-ewoc -1))))
        (t
         ;; Point is elsewhere: move to previous entry.
         (hyperdrive-dir-move (- n)))))

(cl-defun hyperdrive-dir-move (&optional (n 1))
  "Move forward N entries."
  (let ((next-fn (pcase n
                   ((pred (< 0)) #'ewoc-next)
                   ((pred (> 0)) #'ewoc-prev)))
        (node (ewoc-locate hyperdrive-dir-ewoc))
        (i 0)
        (n (abs n))
        target-node)
    (while (and (< i n)
                (setf node (funcall next-fn hyperdrive-dir-ewoc node)))
      (setf target-node node)
      (cl-incf i))
    (when target-node
      (goto-char (ewoc-location target-node)))))

(provide 'hyperdrive-dir)
;;; hyperdrive-dir.el ends here
