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
(defvar hyperdrive-timestamp-format)

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

;; (defun hyperdrive--ewoc-last-matching (ewoc predicate)
;;   "Return the last node in EWOC matching PREDICATE.
;; PREDICATE is called with node's data.  Searches backward from
;; last node."
;;   ;; Copied from `ement-room--ewoc-last-matching'.
;;   (declare (indent defun))
;;   ;; Intended to be like `ewoc-collect', but returning as soon as a match is found.
;;   (cl-loop with node = (ewoc-nth ewoc -1)
;;            while node
;;            when (funcall predicate (ewoc-data node))
;;            return node
;;            do (setf node (ewoc-prev ewoc node))))

(defun hyperdrive-ewoc-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  (pcase-exhaustive thing
    ((pred hyperdrive-entry-p)
     (insert (hyperdrive-ewoc--format-entry thing)))))

(defun hyperdrive-ewoc--format-entry (entry)
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

(defvar-keymap hyperdrive-ewoc-mode-map
  :parent  special-mode-map
  :doc "Local keymap for `hyperdrive-ewoc-mode' buffers."
  "RET" #'hyperdrive-ewoc-find-file
  "^"   #'hyperdrive-ewoc-up-directory
  "w"   #'hyperdrive-ewoc-copy-filename-as-kill
  "n"   #'hyperdrive-ewoc-next
  "p"   #'hyperdrive-ewoc-previous
  "D"   #'hyperdrive-ewoc-delete)

(declare-function hyperdrive-revert-buffer "hyperdrive")

(define-derived-mode hyperdrive-ewoc-mode special-mode
  `("Hyperdrive-EWOC"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive directory buffers."
  :interactive nil
  (let ((inhibit-read-only t))
    (erase-buffer))
  (hl-line-mode)
  (setq-local revert-buffer-function #'hyperdrive-revert-buffer)
  (setf hyperdrive-ewoc (ewoc-create #'hyperdrive-ewoc-pp)
        ;; TODO(alphapapa): Imenu support.
        ;; imenu-create-index-function #'ement-room--imenu-create-index-function
        ))

;;;; Commands

;; TODO: [#A] Implement sorting by size, type, etc.

(declare-function hyperdrive-open "hyperdrive")

(defun hyperdrive-ewoc-find-file (entry)
  "Find ENTRY at point.
Switch to a buffer visiting ENTRY, creating one if none already
exists."
  (interactive (list (ewoc-data (ewoc-locate hyperdrive-ewoc))))
  (hyperdrive-open (hyperdrive-entry-url entry)))

(defun hyperdrive-ewoc-up-directory ()
  "Go up to parent directory."
  (interactive)
  (if-let ((parent-url (hyperdrive--parent (hyperdrive-entry-url hyperdrive-current-entry))))
      (hyperdrive-open parent-url)
    (user-error "At root directory")))

(defun hyperdrive-ewoc-copy-filename-as-kill (entry)
  "Copy URL of file at point into the kill ring."
  ;; TODO: Copy current directory-entry when cursor is on header (or
  ;;       footer?). This may be be awkward UI, since `ewoc-previous'
  ;;       will never land point on the header.
  (interactive (list (ewoc-data (ewoc-locate hyperdrive-ewoc))))
  (let ((url (hyperdrive-entry-url entry)))
    (kill-new url)
    (hyperdrive-message "%s" url)))

(defun hyperdrive-ewoc-delete (entry)
  "Delete ENTRY."
  (interactive (list (ewoc-data (ewoc-locate hyperdrive-ewoc))))
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

(cl-defun hyperdrive-ewoc-next (&optional (n 1))
  "Move forward N entries."
  (interactive "p")
  (let ((next-fn (pcase n
                   ((pred (< 0)) #'ewoc-next)
                   ((pred (> 0)) #'ewoc-prev)))
        (node (ewoc-locate hyperdrive-ewoc))
        (i 0)
        (n (abs n))
        target-node)
    (while (and (< i n)
                (setf node (funcall next-fn hyperdrive-ewoc node)))
      (setf target-node node)
      (cl-incf i))
    (when target-node
      (goto-char (ewoc-location target-node)))))

(cl-defun hyperdrive-ewoc-previous (&optional (n 1))
  "Move backward N entries."
  (interactive "p")
  (hyperdrive-ewoc-next (- n)))

(provide 'hyperdrive-ewoc)
;;; hyperdrive-ewoc.el ends here
