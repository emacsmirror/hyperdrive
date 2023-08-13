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
          ((or (> current-line last-line)
               (and hyperdrive-column-headers (= 2 current-line)))
           ;; Point is below the last entry or on column headers: signal error.
           (hyperdrive-user-error "No file on this line"))
          (t
           ;; Point on a file entry: return its entry.
           entry-at-point))))

;;;; Mode

(declare-function hyperdrive-find-file "hyperdrive")
(declare-function hyperdrive-up "hyperdrive")
(declare-function hyperdrive-describe-hyperdrive "hyperdrive-describe")

(defvar-keymap hyperdrive-dir-mode-map
  :parent hyperdrive-ewoc-mode-map
  :doc "Local keymap for `hyperdrive-dir-mode' buffers."
  "RET" #'hyperdrive-dir-find-file
  "f"   #'hyperdrive-find-file ;; Alternatively, define new function which fills in name of entry at point.
  "j"   #'imenu
  "w"   #'hyperdrive-dir-copy-url
  "d"   #'hyperdrive-dir-download-file
  "^"   #'hyperdrive-up
  "D"   #'hyperdrive-dir-delete
  "H"   #'hyperdrive-dir-history
  "?"   #'hyperdrive-describe-hyperdrive)

;; TODO: Get rid of this?
(declare-function hyperdrive-bookmark-make-record "hyperdrive")

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

;; TODO: Implement sorting by size, type, etc.

(declare-function hyperdrive-open "hyperdrive")

(defun hyperdrive-dir-find-file (entry)
  "Visit hyperdrive ENTRY at point.
Interactively, visit file or directory at point in
`hyperdrive-dir' buffer."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (hyperdrive-open entry))

(declare-function hyperdrive-copy-url "hyperdrive")

(defun hyperdrive-dir-copy-url (entry)
  "Copy URL of ENTRY into the kill ring."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (hyperdrive-copy-url entry))

(declare-function hyperdrive-download-entry "hyperdrive")

(defun hyperdrive-dir-download-file (entry filename)
  "Download ENTRY at point to FILENAME on disk."
  (declare (modes hyperdrive-dir-mode))
  (interactive
   (pcase-let* ((entry (hyperdrive-dir--entry-at-point))
                ((cl-struct hyperdrive-entry name) entry)
                (read-filename (read-file-name "Filename: " (expand-file-name name hyperdrive-download-directory))))
     (list entry read-filename)))
  (hyperdrive-download-entry entry filename))

(defun hyperdrive-dir-delete (entry)
  "Delete ENTRY."
  (declare (modes hyperdrive-dir-mode))
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (when (eq entry hyperdrive-current-entry)
    ;; TODO: Also prevent deletion of "..".
    (hyperdrive-user-error "Won't delete parent directory from within"))
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

(declare-function hyperdrive-history "hyperdrive-history")

(defun hyperdrive-dir-history (entry)
  "Display version history for ENTRY at point."
  (interactive (list (hyperdrive-dir--entry-at-point)))
  (hyperdrive-history entry))

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

(provide 'hyperdrive-dir)
;;; hyperdrive-dir.el ends here
