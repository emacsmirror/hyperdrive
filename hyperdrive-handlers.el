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

(require 'url)

(require 'hyperdrive-dir)
(require 'hyperdrive-lib)

;;;; Variables

(defvar eww-use-browse-url)

;;;; Handlers

(defvar hyperdrive-type-handlers
  `(
    ;; Directories are sent from the gateway as JSON arrays
    ("application/json" . hyperdrive-handler-json)
    (,(rx bos "audio/") . hyperdrive-handler-streamable)
    (,(rx bos "video/") . hyperdrive-handler-streamable)
    (,(rx (or "text/html" "application/xhtml+xml")) . hyperdrive-handler-html))
  "Alist mapping MIME types to handler functions.
Keys are regexps matched against MIME types.")

(declare-function hyperdrive--org-link-goto "hyperdrive-org")
(cl-defun hyperdrive-handler-default (entry &key then)
  "Load ENTRY's file into an Emacs buffer.
If then, then call THEN with no arguments.  Default handler."
  (hyperdrive-api 'get (hyperdrive-entry-url entry)
    :noquery t
    :as (lambda ()
          (pcase-let* (((cl-struct hyperdrive-entry hyperdrive version etc) entry)
                       ((map target) etc)
                       (response-buffer (current-buffer))
                       (inhibit-read-only t))
            ;; TODO: Revisit buffer naming/"visiting" (e.g. what
            ;; happens if the user opens a Hyperdrive file and then
            ;; saves another buffer to the same location?).  See
            ;; also: hyperdrive-save, etc.
            (with-current-buffer (hyperdrive--get-buffer-create entry)
              ;; TODO: Don't reload if we're jumping to a link on the
              ;; same page (but ensure that reverting still works).
              (if (buffer-modified-p)
                  (hyperdrive-message "Buffer modified: %S" (current-buffer))
                (erase-buffer)
                (insert-buffer-substring response-buffer)
                (setf buffer-undo-list nil
                      buffer-read-only (or (not (hyperdrive-writablep hyperdrive)) version))
                (set-buffer-modified-p nil)
                (set-visited-file-modtime (current-time))
                (goto-char (point-min)))
              ;; TODO: Option to defer showing buffer.
              ;; It seems that `pop-to-buffer' is moving point, even
              ;; though it shouldn't, so we call it here, before going
              ;; to a link target.
              (pop-to-buffer (current-buffer))
              (when target
                (pcase major-mode
                  ('org-mode
                   (require 'hyperdrive-org)
                   (hyperdrive--org-link-goto target))
                  ('markdown-mode
                   ;; TODO: Handle markdown link
                   )))
              (when then
                (funcall then)))))))

(cl-defun hyperdrive-handler-directory (directory-entry &key then)
  "Show DIRECTORY-ENTRY.
If THEN, then call THEN in the directory buffer with no
arguments."
  ;; NOTE: ENTRY is not necessarily "filled" yet.
  ;; TODO: Set a timer and say "Opening URL..." if entry doesn't load
  ;; in a couple of seconds (same in hyperdrive-handler-default)
  ;; (like new with-delayed-message ?)
  ;; FIXME: The `queue' which is stored inside the entry has a closure
  ;; which cyclically references the entry itself. Elsewhere, we call
  ;; `hyperdrive-copy-tree' on the entry, leading to the error about
  ;; nesting exceeding `max-lisp-eval-depth'.
  (cl-symbol-macrolet ((queue (alist-get 'fill-queue (hyperdrive-entry-etc directory-entry))))
    (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path version)
                  directory-entry)
                 (url (hyperdrive-entry-url directory-entry))
                 (inhibit-read-only t)
                 ((cl-struct plz-response headers body)
                  ;; SOMEDAY: Consider updating plz to optionally not stringify the body.
                  (hyperdrive-api 'get url :as 'response :noquery t))
                 (entry-names (json-read-from-string body))
                 (entries
                  (mapcar (lambda (entry-name)
                            (hyperdrive-entry-create
                             :hyperdrive hyperdrive
                             :path (concat (url-unhex-string path) entry-name)
                             :version version
                             :encode t))
                          entry-names))
                 (parent-entry (hyperdrive-parent directory-entry))
                 (main-header (hyperdrive-entry-description directory-entry))
                 (header
                  (if hyperdrive-column-headers
                      (concat main-header "\n"
                              (format "%6s  %s  %s"
                                      (propertize "Size" 'face 'hyperdrive-column-header)
                                      (format hyperdrive-timestamp-format-string
                                              (propertize "Last Modified" 'face 'hyperdrive-column-header))
                                      (propertize "Name" 'face 'hyperdrive-column-header)))
                    main-header))
                 (ewoc))
      (when parent-entry
        (setf (alist-get 'display-name (hyperdrive-entry-etc parent-entry))  "..")
        (push parent-entry entries))
      (setf directory-entry (hyperdrive--fill directory-entry headers))
      (hyperdrive-fill-metadata hyperdrive)
      (with-current-buffer (hyperdrive--get-buffer-create directory-entry)
        ;; (when (and (bound-and-true-p hyperdrive-ewoc)
        ;;            (ewoc-nth hyperdrive-ewoc 0))
        ;;   ;; When EWOC has nodes, remember the current node and line so
        ;;   ;; we can try to keep point.
        ;;   (setf prev-node-data (ewoc-data (ewoc-locate hyperdrive-ewoc)) ;;         prev-line (line-number-at-pos)))
        (setf ewoc hyperdrive-ewoc) ; Bind this for the hyperdrive-fill lambda.
        (ewoc-filter hyperdrive-ewoc #'ignore)
        (erase-buffer)
        (ewoc-set-hf hyperdrive-ewoc header "")
        (mapc (lambda (entry)
                (ewoc-enter-last hyperdrive-ewoc entry))
              entries)
        ;; (when prev-node-data
        ;;   ;; Try to return point to where it was before reverting the buffer.
        ;;   ;; FIXME: This doesn't always work correctly, apparently due
        ;;   ;; to the async filling of entries and refreshing of the EWOC.
        ;;   ;; (if-let ((node (hyperdrive--ewoc-last-matching hyperdrive-ewoc
        ;;   ;;                  (lambda (node-data)
        ;;   ;;                    (hyperdrive-entry-equal prev-node-data node-data)))))
        ;;   ;;     (goto-char (ewoc-location node))
        ;;   ;;   (goto-char (point-min))
        ;;   ;;   (forward-line (1- prev-line)))
        ;;   (goto-char (point-min))
        ;;   (forward-line (1- prev-line)))
        (display-buffer (current-buffer) hyperdrive-directory-display-buffer-action)
        (when queue
          (plz-clear queue))
        (setf queue
              (make-plz-queue :limit 8
                              :finally (lambda ()
                                         ;; NOTE: Ensure that the buffer's window is selected,
                                         ;; if it has one.  (Workaround a possible bug in EWOC.)
                                         (if-let ((buffer-window (get-buffer-window (ewoc-buffer ewoc))))
                                             (with-selected-window buffer-window
                                               ;; TODO: Use `ewoc-invalidate' on individual entries
                                               ;; (maybe later, as performance comes to matter more).
                                               (ewoc-refresh hyperdrive-ewoc)
                                               (goto-char (point-min))
                                               (set-buffer-modified-p nil))
                                           (with-current-buffer (ewoc-buffer ewoc)
                                             (ewoc-refresh hyperdrive-ewoc)
                                             (goto-char (point-min))
                                             (set-buffer-modified-p nil)))
                                         (with-current-buffer (ewoc-buffer ewoc)
                                           (when then
                                             (funcall then))))))
        (mapc (lambda (entry)
                ;; TODO: Handle failures?
                (hyperdrive-fill entry :queue queue :then #'ignore))
              entries)
        (set-buffer-modified-p nil)
        (goto-char (point-min))))))

(cl-defun hyperdrive-handler-streamable (entry &key _then)
  ;; TODO: Is there any reason to not pass THEN through?
  ;; FIXME: Opening a streamable entry from a hyperdrive-dir buffer
  ;; buries the -dir buffer.
  "Stream ENTRY."
  (hyperdrive-message (format "Streaming %s..." (hyperdrive--format-entry-url entry)))
  (pcase-let ((`(,command . ,args)
               (split-string hyperdrive-stream-player-command)))
    (apply #'start-process "hyperdrive-stream-player"
           nil command (cl-substitute (hyperdrive--httpify-url
                                       (hyperdrive-entry-url entry))
                                      "%s" args :test #'equal))))

(cl-defun hyperdrive-handler-json (entry &key then)
  "Show ENTRY.
THEN is passed to other handlers, which see.  If ENTRY is a
directory (if its URL ends in \"/\"), pass to
`hyperdrive-handler-directory'.  Otherwise, open with
`hyperdrive-handler-default'."
  (if (hyperdrive--entry-directory-p entry)
      (hyperdrive-handler-directory entry :then then)
    (hyperdrive-handler-default entry :then then)))

(cl-defun hyperdrive-handler-html (entry &key then)
  "Show ENTRY, where ENTRY is an HTML file.
Renders HTML with `shr-insert-document', then calls THEN if
given."
  (if hyperdrive-render-html
      (progn
        (eww (hyperdrive-entry-url entry))
        (when then
          (funcall then)))
    (hyperdrive-handler-default entry :then then)))

(defun hyperdrive-url-loader (parsed-url)
  "Retrieve URL synchronously.
URL must be a parsed URL.  See `url-generic-parse-url' for details.

The return value of this function is the retrieval buffer."
  (cl-check-type parsed-url url "Need a pre-parsed URL.")
  (let* ((url (url-recreate-url parsed-url))
         ;; response-buffer will contain the loaded HTML, and will be deleted at the end of `eww-render'.
         (response-buffer (hyperdrive-api 'get url :as 'buffer)))
    (with-current-buffer response-buffer
      (widen)
      (goto-char (point-min))
      (while (search-forward (string ?\C-m) nil t)
        ;; Strip CRLF from headers so that `eww-parse-headers' works correctly.
        ;; MAYBE: As an alternative, look at buffer coding systems to
        ;; make `eww-parse-headers' work with CRLFs (since according
        ;; to the HTTP 1 spec, headers are supposed to end with CRLF)
        (replace-match ""))
      (current-buffer))))

;;;; Configure Emacs and EWW for hyper:// URLs.

(puthash "hyper" '(name "hyper" loader hyperdrive-url-loader
                        ;; Expand relative paths against host
                        expand-file-name url-default-expander)
         url-scheme-registry)

(when (version<= "28.1" emacs-version)
  (require 'eww)
  (setf eww-use-browse-url
        (if eww-use-browse-url
            (rx-to-string `(or ,eww-use-browse-url (seq bos "hyper://")))
          (rx bos "hyper://"))))

;;;; Footer

(provide 'hyperdrive-handlers)
;;; hyperdrive-handlers.el ends here
