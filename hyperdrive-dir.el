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

(require 'bookmark)
(require 'dired)  ; For faces.

(require 'hyperdrive-lib)
(require 'hyperdrive-ewoc)

;;;; Variables

(defvar hyperdrive-current-entry)
(defvar hyperdrive-timestamp-format)
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

(declare-function hyperdrive-up "hyperdrive")
(declare-function hyperdrive-describe-hyperdrive "hyperdrive")

(defvar-keymap hyperdrive-dir-mode-map
  :parent hyperdrive-ewoc-mode-map
  :doc "Local keymap for `hyperdrive-dir-mode' buffers."
  "RET" #'hyperdrive-dir-find-file
  "w"   #'hyperdrive-dir-copy-url
  "d"   #'hyperdrive-dir-download-file
  "^"   #'hyperdrive-up
  "D"   #'hyperdrive-dir-delete
  "?"   #'hyperdrive-describe-hyperdrive)

(declare-function hyperdrive-bookmark-make-record "hyperdrive")

(define-derived-mode hyperdrive-dir-mode hyperdrive-ewoc-mode
  `("Hyperdrive-dir"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive directory buffers."
  (setf hyperdrive-ewoc (ewoc-create #'hyperdrive-dir-pp)
        ;; TODO(alphapapa): Imenu support.
        ;; imenu-create-index-function #'ement-room--imenu-create-index-function
        ))

;;;; Commands

;; TODO: Implement sorting by size, type, etc.

(declare-function hyperdrive-open "hyperdrive")

(defun hyperdrive-dir-find-file (entry)
  "Visit hyperdrive ENTRY at point.
Interactively, visit file or directory at point in
`hyperdrive-dir' buffer."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (when entry (hyperdrive-open entry)))

(declare-function hyperdrive-copy-url "hyperdrive")

(defun hyperdrive-dir-copy-url (entry)
  "Copy URL of ENTRY into the kill ring."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (when entry (hyperdrive-copy-url entry)))

(declare-function hyperdrive-download-entry "hyperdrive")

(defun hyperdrive-dir-download-file (entry filename)
  "Download ENTRY at point to FILENAME on disk."
  (declare (modes hyperdrive-dir-mode))
  (interactive
   (pcase-let* ((entry (hyperdrive-dir--entry-at-point))
                ((cl-struct hyperdrive-entry name) entry)
                (read-filename (read-file-name "Filename: " (expand-file-name name hyperdrive-download-directory))))
     (list entry read-filename)))
  (when entry (hyperdrive-download-entry entry filename)))

(defun hyperdrive-dir--entry-at-point ()
  "Return entry at point.
With point below last entry, returns nil.
With point on header, returns directory entry."
  (cond ((= 1 (line-number-at-pos))
         ;; Point on header: return directory's entry.
         hyperdrive-current-entry)
        ((> (line-number-at-pos)
            (line-number-at-pos (ewoc-location (ewoc-nth hyperdrive-ewoc -1))))
         ;; Point is below the last entry: return nil.
         nil)
        (t
         ;; Point on a file entry: return its entry.
         (ewoc-data (ewoc-locate hyperdrive-ewoc)))))

(defun hyperdrive-dir-delete (entry)
  "Delete ENTRY."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (ewoc-data (ewoc-locate hyperdrive-ewoc))))
  (pcase-let (((cl-struct hyperdrive-entry name) entry)
              (buffer (current-buffer)))
    (when (and (yes-or-no-p (format "Delete %S? " name))
               (or (not (hyperdrive--entry-directory-p entry))
                   (yes-or-no-p (format "Recursively delete %S? " name))))
      (hyperdrive-delete entry
        :then (lambda (_)
                (when (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (revert-buffer)))
                (hyperdrive-message "Deleted: %S (Deleted files can be accessed from prior versions of the hyperdrive.)" name))
        :else (lambda (plz-error)
                (pcase-let* (((cl-struct plz-error response) plz-error)
                             ((cl-struct plz-response status) response)
                             (message
                              (pcase status
                                (403 "Hyperdrive not writable")
                                (405 "Cannot write to old version")
                                (_ plz-error))))
                  (hyperdrive-message "Unable to delete: %S: %S" name message)))))))

(provide 'hyperdrive-dir)
;;; hyperdrive-dir.el ends here
