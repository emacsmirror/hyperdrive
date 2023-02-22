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
(require 'ewoc)

(require 'hyperdrive)

;;;; Variables

(defvar-local hyperdrive-current-url nil
  "URL of current buffer.")

(defvar-local hyperdrive-ewoc nil
  "EWOC for current hyperdrive buffer.")

(defvar-local hyperdrive-entries nil
  "Entries in current hyperdrive buffer.")

;;;; Structs

(cl-defstruct hyperdrive-entry
  "Represents an entry in a hyperdrive."
  (url nil :documentation "URL to entry's parent directory (i.e. does not include name).")
  (name nil :documentation "Name of entry.")
  (headers nil :documentation "HTTP headers from request.")
  (modified nil :documentation "Last modified time.")
  (size nil :documentation "Size of file.")
  (version nil :documentation "Version of the file (if applicable).")
  (type nil :documentation "MIME type of the entry."))

(cl-defstruct hyperdrive-directory
  "Represents a directory in a hyperdrive."
  (headers nil :documentation "HTTP headers from request.")
  (modified nil :documentation "Last modified time.")
  (url nil :documentation "URL returned by gateway.")
  (entries nil :documentation "Entries in the directory."))

;;;; Functions

(defun hyperdrive-ewoc-list (url)
  "List URL in Hyperdrive buffer."
  (unless (string-suffix-p "/" url)
    (user-error "URL is not to a directory: %s" url))
  (with-current-buffer (hyperdrive--get-buffer-create url)
    (hyperdrive-ewoc-mode)
    (pcase-let* ((inhibit-read-only t)
                 ((cl-struct plz-response body)
                  (hyperdrive-api 'get url :as 'response))
                 (entries (mapcar (lambda (entry)
                                    (make-hyperdrive-entry :url url :name entry))
                                  (json-read-from-string body)))
                 (ewoc hyperdrive-ewoc))
      (erase-buffer)
      (setf hyperdrive-entries entries)
      (mapc (lambda (entry)
              (ewoc-enter-last hyperdrive-ewoc entry))
            entries)
      (mapc (lambda (entry)
              (hyperdrive-fill-entry entry
                                     (lambda (_)
                                       ;; NOTE: Ensure that the buffer's window is selected,
                                       ;; if it has one.  (Workaround a possible bug in EWOC.)
                                       (if-let ((buffer-window (get-buffer-window (ewoc-buffer ewoc))))
                                           (with-selected-window buffer-window
                                             ;; TODO: Use `ewoc-invalidate' on individual entries
                                             ;; (maybe later, as performance comes to matter more).
                                             (ewoc-refresh ewoc))
                                         (ewoc-refresh ewoc)))))
            entries)
      (pop-to-buffer (current-buffer)))))

(defun hyperdrive-fill-entry (entry &optional then)
  "Fill ENTRY's metadata and call THEN."
  ;; TODO(alphapapa): Factor this out of -ewoc.el.
  (let ((callback (lambda (response)
                    (pcase-let* (((cl-struct plz-response headers) response)
                                 ((map last-modified) headers))
                      (setf (hyperdrive-entry-modified entry) last-modified)
                      (funcall then entry))))
        (url (concat (hyperdrive-entry-url entry) "/" (hyperdrive-entry-name entry))))
    (hyperdrive-api 'head url :as 'response :then callback :else #'ignore)))

(defun hyperdrive-ewoc-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  (pcase-exhaustive thing
    ((pred hyperdrive-entry-p)
     (insert (hyperdrive-ewoc--format-entry thing)))
    ((pred hyperdrive-directory-p))))

(defun hyperdrive-ewoc--format-entry (entry)
  "Return ENTRY formatted as a string."
  (format "%-40s %s"
          (hyperdrive-entry-name entry)
          (or (hyperdrive-entry-modified entry) "")))

;;;; Mode

(defvar-keymap hyperdrive-ewoc-mode-map
  :parent  special-mode-map
  :doc "Local keymap for `hyperdrive-ewoc-mode' buffers."
  "RET"     #'hyperdrive-ewoc-find-file
  ;; TODO(alphapapa): Port these commands.
  ;; "^"       #'hyperdrive-up-directory
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
  (remove-overlays)
  (setf buffer-read-only t
        ;; TODO(alphapapa): Imenu support.
        ;; imenu-create-index-function #'ement-room--imenu-create-index-function
        hyperdrive-ewoc (ewoc-create #'hyperdrive-ewoc-pp)))

;;;; Commands

(defun hyperdrive-ewoc-find-file (entry)
  "Find ENTRY at point."
  (interactive (list (ewoc-data (ewoc-locate hyperdrive-ewoc))))
  (hyperdrive-load-url (concat (hyperdrive-entry-url entry)
                               (hyperdrive-entry-name entry))))

(provide 'hyperdrive-ewoc)
;;; hyperdrive-ewoc.el ends here
