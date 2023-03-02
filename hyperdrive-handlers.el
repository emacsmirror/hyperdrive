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
(defvar hyperdrive-timestamp-format)
(defvar hyperdrive-directory-display-buffer-action)

;;;; Handlers

(defvar hyperdrive-type-handlers
  '(("application/json" . hyperdrive-handler-json)
    ("\\`audio/" . hyperdrive-handler-streamable)
    ("\\`video/" . hyperdrive-handler-streamable))
  "Alist mapping MIME types to handler functions.
Keys are regexps matched against MIME types.")

(declare-function hyperdrive-mode "hyperdrive")

(cl-defun hyperdrive-handler-default (entry &key then)
  "Load ENTRY's file into an Emacs buffer.
Default handler."
  ;; FIXME: Make buffer read-only when hyperdrive isn't writable.
  (hyperdrive-api 'get (hyperdrive-entry-url entry)
    :as (lambda ()
          (let ((response-buffer (current-buffer))
                (inhibit-read-only t))
            ;; TODO: Revisit buffer naming/"visiting" (e.g. what
            ;; happens if the user opens a Hyperdrive file and then
            ;; saves another buffer to the same location?).  See
            ;; also: hyperdrive-save, etc.
            (switch-to-buffer (hyperdrive--get-buffer-create entry))
            (when (buffer-modified-p)
              (error "Hyperdrive: Buffer modified: %S" (current-buffer)))
            (erase-buffer)
            (insert-buffer-substring response-buffer)
            (goto-char (point-min))
            ;; Inspired by https://emacs.stackexchange.com/a/2555/39549
            (when hyperdrive-honor-auto-mode-alist
              (let ((buffer-file-name (hyperdrive-entry-url entry)))
                (set-auto-mode)))
            ;; TODO: Option to defer showing buffer.
            (hyperdrive-mode)
            (set-buffer-modified-p nil)
            ;; FIXME: Do this in a wrapper.
            ;; (when target
            ;;   ;; FIXME: This is specific to Org files and doesn't
            ;;   ;; quite belong here.  (OTOH we could use this
            ;;   ;; function to find text in non-Org files, too, I
            ;;   ;; think.)
            ;;   (require 'ol)
            ;;   (org-link-search target))
            (pop-to-buffer (current-buffer))
            (when then
              (funcall then))))))

(declare-function hyperdrive-ewoc-mode "hyperdrive-ewoc")

(cl-defun hyperdrive-handler-directory (directory-entry &key then)
  "Show directory ENTRY."
  ;; TODO: Open in same window (but probably still new buffer) (display-buffer-alist ?)
  ;; NOTE: ENTRY is not necessarily "filled" yet.
  ;; FIXME: About half of the time, calls to hyperdrive-ewoc-list
  ;; fail. Issue with sending many rapid HEAD requests?
  ;; TODO: Refactor some of this code to -ewoc, or something like that, depending...
  (pcase-let* ((url (hyperdrive-entry-url directory-entry))
               (buffer (hyperdrive--get-buffer-create directory-entry))
               (inhibit-read-only t)
               ((cl-struct plz-response headers body)
                ;; SOMEDAY: Consider updating plz to optionally not stringify the body.
                (hyperdrive-api 'get url :as 'response))
               (entry-names (json-read-from-string body))
               (entries
                (mapcar (lambda (entry-name)
                          (make-hyperdrive-entry
                           :hyperdrive (hyperdrive-entry-hyperdrive directory-entry)
                           ;; Rather than storing just the path and making a function to return
                           ;; the name, we store the name as-is because, for one thing, the name
                           ;; could theoretically contain a slash, and `file-name-nondirectory'
                           ;; would return the wrong value in that case.
                           :path (concat (hyperdrive-entry-path directory-entry) entry-name)
                           :name entry-name))
                        entry-names))
               ;; TODO: Consider adding a working-directory-entry. It more closely resembles
               ;;       dired, and it would be useful to put the point on something
               ;;       representing the current directory, enabling users to copy a link to
               ;;       the working directory with `hyperdrive-ewoc-copy-filename-as-kill' or
               ;;       click a visible button to reload the current directory (an alternative
               ;;       to `hyperdrive-revert-buffer'). `hyperdrive-ewoc-previous' does not
               ;;       allow user to move point to the header.
               (parent-url (hyperdrive--parent url))
               (parent-entry (when parent-url
                               (hyperdrive-url-entry parent-url)))
               (ewoc)
               (header))
    (when parent-entry
      (setf (alist-get 'display-name (hyperdrive-entry-etc parent-entry))  "..")
      (push parent-entry entries))
    (setf directory-entry (hyperdrive--fill directory-entry headers)
          hyperdrive-entries entries
          header (format "%s (%s)"
                         (propertize (hyperdrive--format-url url)
                                     'face 'hyperdrive-header)
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
              (hyperdrive-fill entry
                :then (lambda (_)
                        ;; NOTE: Ensure that the buffer's window is selected,
                        ;; if it has one.  (Workaround a possible bug in EWOC.)
                        (if-let ((buffer-window (get-buffer-window (ewoc-buffer ewoc))))
                            (with-selected-window buffer-window
                              ;; TODO: Use `ewoc-invalidate' on individual entries
                              ;; (maybe later, as performance comes to matter more).
                              (ewoc-refresh hyperdrive-ewoc))
                          (with-current-buffer (ewoc-buffer ewoc)
                            (ewoc-refresh hyperdrive-ewoc))))))
            entries)
      (set-buffer-modified-p nil)
      (display-buffer (current-buffer) hyperdrive-directory-display-buffer-action)
      (when then
        (funcall then)))))

(cl-defun hyperdrive-handler-streamable (entry &key _then)
  "Stream ENTRY."
  (mpv-play-url (hyperdrive--httpify-url (hyperdrive-entry-url entry))))

(cl-defun hyperdrive-handler-json (entry &key _then)
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
