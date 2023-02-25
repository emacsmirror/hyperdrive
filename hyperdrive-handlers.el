;;; hyperdrive-handlers.el --- Handlers for entry types  -*- lexical-binding: t; -*-

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

(require 'ewoc)

(require 'hyperdrive-lib)

(defvar hyperdrive-honor-auto-mode-alist)
(defvar hyperdrive-ewoc)
(defvar hyperdrive-entries)

;;;; Handlers

(defvar hyperdrive-type-handlers
  '(("application/json" . hyperdrive-handler-json)
    ("\\`audio/" . hyperdrive-handler-streamable)
    ("\\`video/" . hyperdrive-handler-streamable))
  "Alist mapping MIME types to handler functions.
Keys are regexps matched against MIME types.")

(declare-function hyperdrive-mode "hyperdrive")

(defun hyperdrive-handler-default (entry)
  "Load ENTRY's file into an Emacs buffer.
Default handler."
  (pcase-let (((cl-struct hyperdrive-entry url) entry))
    (hyperdrive-api 'get url
      :as (lambda ()
            (let ((response-buffer (current-buffer))
                  (inhibit-read-only t))
              (with-current-buffer (hyperdrive--get-buffer-create entry)
                (erase-buffer)
                (insert-buffer-substring response-buffer)
                ;; Inspired by https://emacs.stackexchange.com/a/2555/39549
                (when hyperdrive-honor-auto-mode-alist
                  (let ((buffer-file-name (hyperdrive-entry-url entry)))
                    (set-auto-mode)))
                ;; TODO: Option to defer showing buffer.
                (hyperdrive-mode)
                (pop-to-buffer (current-buffer))))))))

(declare-function hyperdrive-ewoc-mode "hyperdrive-ewoc")

(defun hyperdrive-handler-directory (directory-entry)
  "Show directory ENTRY."
  ;; NOTE: ENTRY is not necessarily "filled" yet.
  ;; FIXME: About half of the time, calls to hyperdrive-ewoc-list
  ;; fail. Issue with sending many rapid HEAD requests?
  (pcase-let* (((cl-struct hyperdrive-entry url) directory-entry)
               (buffer (hyperdrive--get-buffer-create directory-entry))
               (inhibit-read-only t)
               ((cl-struct plz-response headers body)
                ;; SOMEDAY: Consider updating plz to optionally not stringify the body.
                (hyperdrive-api 'get url :as 'response))
               (encoded-entry-names (json-read-from-string body))
               (entries
                (mapcar (lambda (encoded-entry-name)
                          (let ((entry-url (concat url encoded-entry-name)))
                            (make-hyperdrive-entry :url entry-url
                                                   :name (url-unhex-string encoded-entry-name))))
                        encoded-entry-names))
               (parent-url (hyperdrive--parent-url directory-entry))
               (ewoc)
               (header))
    (when parent-url
      (push (make-hyperdrive-entry :url parent-url
                                   :etc '((display-name . "..")))
            entries))
    (setf directory-entry (hyperdrive--fill-entry directory-entry headers)
          hyperdrive-entries entries
          header (format "%s (%s)"
                         (hyperdrive--format-url url)
                         (hyperdrive-entry-etag directory-entry)))
    (with-current-buffer buffer
      (hyperdrive-ewoc-mode)
      (setf ewoc hyperdrive-ewoc) ; Bind this for the fill-entry lambda.
      (ewoc-filter hyperdrive-ewoc #'ignore)
      (ewoc-set-hf hyperdrive-ewoc header "")
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
                        (ewoc-refresh hyperdrive-ewoc))
                    (ewoc-refresh hyperdrive-ewoc)))))
            entries)
      (pop-to-buffer (current-buffer)))))

(defun hyperdrive-handler-streamable (entry)
  "Stream ENTRY."
  (pcase-let (((cl-struct hyperdrive-entry url) entry))
    (mpv-play-url (hyperdrive--httpify-url url))))

(defun hyperdrive-handler-json (entry)
  "Show ENTRY.
If ENTRY is a directory (if its URL ends in \"/\"), pass to
`hyperdrive-handler-directory'.  Otherwise, open with
`hyperdrive-handler-default'."
  (if (hyperdrive--entry-directory-p entry)
      (hyperdrive-handler-directory entry)
    (hyperdrive-handler-default entry)))

;;;; Footer

(provide 'hyperdrive-handlers)
;;; hyperdrive-handlers.el ends here
