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

;; (defun hyperdrive-entry-metadata (url)
;;   "Return entry metadata derived from response headers of HEAD request to URL."
;;   (let* ((headers (plz-response-headers (hyperdrive-api 'head url :as 'response)))
;;          (returned-url (when (string-match (rx "<" (group (one-or-more anything)) ">")
;;                                            (alist-get 'link headers))
;;                          (match-string 1))))
;;     ;; TODO: Use alist instead of plist?
;;     (list :url returned-url
;;           :size (alist-get 'content-length headers)
;;           :version ;; ????
;;           :directoryp (hyperdrive--directory-p returned-url)
;;           :streamablep (string-match (rx (or "audio" "video")) (alist-get 'content-type headers)))))


;;;; Functions

(defun hyperdrive-ewoc-list (url)
  "List URL in Hyperdrive buffer."
  (unless (string-suffix-p "/" url)
    (user-error "URL is not to a directory: %s" url))
  (pcase-let* (((cl-struct plz-response headers body)
                (hyperdrive-api 'get url :as 'response))
               (entries (mapcar (lambda (url entry)
                                  (make-hyperdrive-entry :url url :name entry))
                                (json-read-from-string body))))
    (mapc (lambda (entry)
            (ewoc-enter-last hyperdrive-ewoc entry))
          entries)))

;; (defun hyperdrive-ewoc-insert (ewoc entry)
;;   "Insert ENTRY into EWOC."
;;   (if-let ((first-node (ewoc-nth ewoc 0)))
;;       (ewoc-enter-after )
;;     ))

(define-derived-mode hyperdrive-ewoc-mode fundamental-mode
  `("Hyperdrive-EWOC"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive directory buffers."
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (setf buffer-read-only t
        ;; TODO: Imenu support.
        ;; imenu-create-index-function #'ement-room--imenu-create-index-function
        hyperdrive-ewoc (ewoc-create #'hyperdrive-ewoc-pp)))

(defun hyperdrive-ewoc-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  (pcase-exhaustive thing
    ((pred hyperdrive-entry-p)
     (insert (hyperdrive-ewoc--format-entry thing)))
    ((pred hyperdrive-directory-p))))

(defun hyperdrive-ewoc--format-entry (entry)
  "Return ENTRY formatted as a string."
  (format "%s" (hyperdrive-entry-name)))


(provide 'hyperdrive-ewoc)
;;; hyperdrive-ewoc.el ends here
