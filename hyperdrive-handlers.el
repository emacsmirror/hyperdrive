;;; hyperdrive-handlers.el --- Handlers for entry types  -*- lexical-binding: t; -*-

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

(require 'hyperdrive-dir)
(require 'hyperdrive-lib)

(defvar hyperdrive-dir)
(defvar hyperdrive-entries)
(defvar hyperdrive-directory-display-buffer-action)
(defvar hyperdrive-stream-player-command)

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
If then, then call THEN with no arguments.  Default handler."
  ;; FIXME: Make buffer read-only when hyperdrive isn't writable.
  (with-local-quit
    (hyperdrive-api 'get (hyperdrive-entry-url entry)
      :noquery t
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
              (setf buffer-undo-list nil)
              (set-buffer-modified-p nil)
              (goto-char (point-min))
              ;; TODO: Option to defer showing buffer.
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
                (funcall then)))))))

(declare-function hyperdrive-dir-mode "hyperdrive-dir")

(cl-defun hyperdrive-handler-directory (directory-entry &key then)
  "Show DIRECTORY-ENTRY.
If then, then call THEN with no arguments."
  ;; NOTE: ENTRY is not necessarily "filled" yet.
  ;; TODO: Refactor some of this code to -ewoc, or something like that, depending...
  (pcase-let* ((url (hyperdrive-entry-url directory-entry))
               (inhibit-read-only t)
               ((cl-struct plz-response headers body)
                ;; SOMEDAY: Consider updating plz to optionally not stringify the body.
                (with-local-quit
                  (hyperdrive-api 'get url :as 'response
                    :noquery t)))
               (entry-names (json-read-from-string body))
               (entries
                (mapcar (lambda (entry-name)
                          (make-hyperdrive-entry
                           :hyperdrive (hyperdrive-entry-hyperdrive directory-entry)
                           :path (concat (hyperdrive-entry-path directory-entry) entry-name)
                           :name entry-name))
                        entry-names))
               (parent-url (hyperdrive--parent url))
               (parent-entry (when parent-url
                               (hyperdrive-url-entry parent-url)))
               (ewoc) (header) ;; (prev-node-data) (prev-line)
               )
    (when parent-entry
      (setf (alist-get 'display-name (hyperdrive-entry-etc parent-entry))  "..")
      (push parent-entry entries))
    (setf directory-entry (hyperdrive--fill directory-entry headers)
          hyperdrive-entries entries
          header (hyperdrive--directory-header directory-entry))
    (setf (hyperdrive-entry-hyperdrive directory-entry)
          (hyperdrive-fill-public-metadata (hyperdrive-entry-hyperdrive directory-entry)))
    (with-current-buffer (hyperdrive--get-buffer-create directory-entry)
      ;; (when (and (bound-and-true-p hyperdrive-dir-ewoc)
      ;;            (ewoc-nth hyperdrive-dir-ewoc 0))
      ;;   ;; When EWOC has nodes, remember the current node and line so
      ;;   ;; we can try to keep point.
      ;;   (setf prev-node-data (ewoc-data (ewoc-locate hyperdrive-dir-ewoc))
      ;;         prev-line (line-number-at-pos)))
      (hyperdrive-dir-mode)
      (setf ewoc hyperdrive-dir-ewoc) ; Bind this for the hyperdrive-fill lambda.
      (ewoc-filter hyperdrive-dir-ewoc #'ignore)
      (ewoc-set-hf hyperdrive-dir-ewoc header "")
      (mapc (lambda (entry)
              (ewoc-enter-last hyperdrive-dir-ewoc entry))
            entries)
      ;; (when prev-node-data
      ;;   ;; Try to return point to where it was before reverting the buffer.
      ;;   ;; FIXME: This doesn't always work correctly, apparently due
      ;;   ;; to the async filling of entries and refreshing of the EWOC.
      ;;   ;; (if-let ((node (hyperdrive--ewoc-last-matching hyperdrive-dir-ewoc
      ;;   ;;                  (lambda (node-data)
      ;;   ;;                    (hyperdrive-entry-equal prev-node-data node-data)))))
      ;;   ;;     (goto-char (ewoc-location node))
      ;;   ;;   (goto-char (point-min))
      ;;   ;;   (forward-line (1- prev-line)))
      ;;   (goto-char (point-min))
      ;;   (forward-line (1- prev-line)))
      (display-buffer (current-buffer) hyperdrive-directory-display-buffer-action)
      (mapc (lambda (entry)
              (hyperdrive-fill entry
                :then (lambda (_)
                        ;; NOTE: Ensure that the buffer's window is selected,
                        ;; if it has one.  (Workaround a possible bug in EWOC.)
                        (if-let ((buffer-window (get-buffer-window (ewoc-buffer ewoc))))
                            (with-selected-window buffer-window
                              ;; TODO: Use `ewoc-invalidate' on individual entries
                              ;; (maybe later, as performance comes to matter more).
                              (ewoc-refresh hyperdrive-dir-ewoc)
                              (goto-char (point-min)))
                          (with-current-buffer (ewoc-buffer ewoc)
                            (ewoc-refresh hyperdrive-dir-ewoc)
                            (goto-char (point-min)))))))
            entries)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (when then
        (funcall then)))))

(defun hyperdrive--directory-header (entry)
  "Return header for ENTRY."
  ;; TODO(A): Ensure that this is shown correctly on first load for DNSlink URLs.
  (let* ((seed (hyperdrive--format-host (hyperdrive-entry-hyperdrive entry)
                                        :format '(seed)))
         (public-name (hyperdrive--format-host (hyperdrive-entry-hyperdrive entry)
                                               :format '(public-name)))
         (handle (cond (seed
                        (concat "seed:" (propertize seed 'face 'hyperdrive-seed)))
                       (public-name
                        ;; TODO: Add public-name face or rename seed face?
                        (concat "name:" (propertize public-name 'face 'hyperdrive-seed)))))
         (url (hyperdrive--format-entry-url entry :host-format '(short-key)))
         (version (hyperdrive-entry-etag entry)))
    (concat (when handle
              (format "(%s) " handle))
            url
            (format " (version:%s)" version))))

(cl-defun hyperdrive-handler-streamable (entry &key _then)
  "Stream ENTRY."
  (pcase-let ((`(,command . ,args)
               (split-string hyperdrive-stream-player-command)))
    (apply #'start-process "hyperdrive-stream-player"
           nil command (cl-substitute (hyperdrive--httpify-url
                                       (hyperdrive-entry-url entry))
                                      "%s" args :test #'equal))))

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
