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

(defvar-local hyperdrive-ewoc nil
  "EWOC for current hyperdrive buffer.")

(defvar-local hyperdrive-entries nil
  "Entries in current hyperdrive buffer.")

;;;; Functions

;;;###autoload
(defun hyperdrive-ewoc-list (directory-url)
  "List DIRECTORY-URL in Hyperdrive buffer."
  ;; FIXME: About half of the time, calls to hyperdrive-ewoc-list fail. Issue with sending many rapid HEAD requests?
  (unless (string-suffix-p "/" directory-url)
    (user-error "URL is not to a directory: %s" directory-url))
  (let* ((directory-entry (make-hyperdrive-entry :url directory-url))
         (buffer (hyperdrive--get-buffer-create directory-entry)))
    (with-current-buffer buffer
      (hyperdrive-ewoc-mode)
      (pcase-let* ((inhibit-read-only t)
                   ((cl-struct plz-response headers body)
                    ;; SOMEDAY: Consider updating plz to optionally not stringify the body.
                    (hyperdrive-api 'get directory-url :as 'response))
                   (encoded-entry-names (json-read-from-string body))
                   (entries (mapcar (lambda (encoded-entry-name)
                                      (let ((entry-url (concat directory-url encoded-entry-name)))
                                        (make-hyperdrive-entry :url entry-url
                                                               :name (url-unhex-string encoded-entry-name))))
                                    encoded-entry-names))
                   (ewoc hyperdrive-ewoc)
                   (parent-url (hyperdrive--parent-url directory-entry)))
        (when parent-url
          (push (make-hyperdrive-entry :url parent-url
                                       :etc '((display-name . "..")))
                entries))
        (setf directory-entry (hyperdrive--fill-entry directory-entry headers)
              hyperdrive-entries entries)
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
        (pop-to-buffer (current-buffer))))))

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
          (or (alist-get 'display-name (hyperdrive-entry-etc entry))
              (hyperdrive-entry-name entry))
          (or (hyperdrive-entry-modified entry) "")))

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
