;;; hyperdrive-ewoc.el --- EWOC frontend for hyperdrive  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USHIN, Inc.

;; Author: Adam Porter <adam@alphapapa.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'dired)  ; For faces.
(require 'ewoc)

(require 'hyperdrive-lib)

;;;; Variables

(defvar-local hyperdrive-ewoc nil
  "EWOC for current hyperdrive buffer.")

(defvar-local hyperdrive-entries nil
  "Entries in current hyperdrive buffer.")

(defvar hyperdrive-current-entry)

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

(defun hyperdrive-ewoc-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  (pcase-exhaustive thing
    ((pred hyperdrive-entry-p)
     (insert (hyperdrive-ewoc--format-entry thing)))
    ((pred hyperdrive-directory-p))))

(defun hyperdrive-ewoc--format-entry (entry)
  "Return ENTRY formatted as a string."
  (pcase-let* (((cl-struct hyperdrive-entry size) entry)
               (size (when size
                       (file-size-human-readable size)))
               (face (if (hyperdrive--entry-directory-p entry)
                         'hyperdrive-directory
                       'default)))
    (format "%-40s  %-6s  %s"
            (propertize (or (alist-get 'display-name (hyperdrive-entry-etc entry))
                            (hyperdrive-entry-name entry))
                        'face face)
            (propertize (or size "")
                        'face 'hyperdrive-size)
            (propertize (or (hyperdrive-entry-modified entry) "")
                        'face 'hyperdrive-timestamp))))

;;;; Mode

(defvar-keymap hyperdrive-ewoc-mode-map
  :parent  special-mode-map
  :doc "Local keymap for `hyperdrive-ewoc-mode' buffers."
  "RET"     #'hyperdrive-ewoc-find-file
  "^"       #'hyperdrive-ewoc-up-directory
  ;; TODO(alphapapa): Port these commands.
  ;; "w"       #'hyperdrive-dired-copy-filename-as-kill
  ;; "D"       #'hyperdrive-dired-delete-file
  )

(define-derived-mode hyperdrive-ewoc-mode fundamental-mode
  `("Hyperdrive-EWOC"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive directory buffers."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (hl-line-mode)
  (setf buffer-read-only t
        ;; TODO(alphapapa): Imenu support.
        ;; imenu-create-index-function #'ement-room--imenu-create-index-function
        hyperdrive-ewoc (ewoc-create #'hyperdrive-ewoc-pp)))

;;;; Commands

(declare-function hyperdrive-open "hyperdrive")

(defun hyperdrive-ewoc-find-file (entry)
  "Find ENTRY at point."
  (interactive (list (ewoc-data (ewoc-locate hyperdrive-ewoc))))
  (hyperdrive-open (hyperdrive-entry-url entry)))

(defun hyperdrive-ewoc-up-directory ()
  "Go up to parent directory."
  (interactive)
  (if-let ((parent-url (hyperdrive--parent-url hyperdrive-current-entry)))
      (hyperdrive-open parent-url)
    (user-error "At root directory")))

(provide 'hyperdrive-ewoc)
;;; hyperdrive-ewoc.el ends here
