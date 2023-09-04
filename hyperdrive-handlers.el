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

;;;; Requirements

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
    (,(rx bos "image/") . hyperdrive-handler-image)
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
                       (response-buffer (current-buffer)))
            ;; TODO: Revisit buffer naming/"visiting" (e.g. what
            ;; happens if the user opens a Hyperdrive file and then
            ;; saves another buffer to the same location?).  See
            ;; also: hyperdrive-save, etc.
            (with-current-buffer (hyperdrive--get-buffer-create entry)
              ;; TODO: Don't reload if we're jumping to a link on the
              ;; same page (but ensure that reverting still works).
              (if (buffer-modified-p)
                  (hyperdrive-message "Buffer modified: %S" (current-buffer))
                (with-silent-modifications
                  (erase-buffer)
                  (insert-buffer-substring response-buffer))
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
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path version) directory-entry)
               (url (hyperdrive-entry-url directory-entry))
               ((cl-struct plz-response headers body)
                ;; SOMEDAY: Consider updating plz to optionally not stringify the body.
                (hyperdrive-api 'get url :as 'response :noquery t))
               (entry-names (json-read-from-string body))
               (entries (mapcar (lambda (entry-name)
                                  (hyperdrive-entry-create
                                   :hyperdrive hyperdrive
                                   :path (concat (url-unhex-string path) entry-name)
                                   :version version
                                   :encode t))
                                entry-names))
               (parent-entry (hyperdrive-parent directory-entry))
               (header (hyperdrive-column-headers (hyperdrive-entry-description directory-entry)))
               (num-entries (length entries)) (num-filled 0)
	       ;; (debug-start-time (current-time))
               (metadata-queue) (ewoc) (prev-entry) (prev-point))
    (cl-labels ((goto-entry (entry ewoc)
                  (when-let ((node (hyperdrive-ewoc-find-node ewoc entry
                                     :predicate #'hyperdrive-entry-equal)))
                    (goto-char (ewoc-location node))))
                (update-footer (num-filled num-of)
                  (when (zerop (mod num-filled 5))
                    (ewoc-set-hf ewoc header
                                 (propertize (format "Loading (%s/%s)..." num-filled num-of)
					     'face 'font-lock-comment-face)))))
      (hyperdrive-fill-metadata hyperdrive)
      (setf directory-entry (hyperdrive--fill directory-entry headers))
      (when parent-entry
        (setf (alist-get 'display-name (hyperdrive-entry-etc parent-entry))  "..")
        (push parent-entry entries))
      (with-current-buffer (hyperdrive--get-buffer-create directory-entry)
        (with-silent-modifications
          (setf ewoc (or hyperdrive-ewoc ; Bind this for lambdas.
                         (setf hyperdrive-ewoc (ewoc-create #'hyperdrive-dir-pp)))
                metadata-queue (make-plz-queue
				;; Experimentation seems to show that a
				;; queue size of about 20 performs best.
                                :limit hyperdrive-queue-size
                                :finally (lambda ()
                                           (with-current-buffer (ewoc-buffer ewoc)
                                             (with-silent-modifications
                                               ;; `with-silent-modifications' increases performance,
                                               ;; but we still need `set-buffer-modified-p' below.
                                               (ewoc-set-hf ewoc header "")
                                               (setf entries (hyperdrive-sort-entries entries))
                                               (dolist (entry entries)
                                                 (ewoc-enter-last ewoc entry))
                                               (or (when prev-entry
                                                     (goto-entry prev-entry ewoc))
                                                   (goto-char prev-point)))
                                             (set-buffer-modified-p nil))
                                           ;; TODO: Remove this and the commented out `debug-start-time'
                                           ;; binding when we're done experimenting.
                                           ;; (message "Elapsed: %s"
                                           ;;          (float-time (time-subtract (current-time)
                                           ;;                                     debug-start-time)))
                                           ))
                prev-entry (when-let ((node (ewoc-locate hyperdrive-ewoc)))
                             (ewoc-data node))
                prev-point (point))
          (ewoc-filter hyperdrive-ewoc #'ignore)
          (update-footer num-filled num-entries)
          (dolist (entry entries)
            (hyperdrive-fill entry :queue metadata-queue
              :then (lambda (&rest _)
                      (update-footer (cl-incf num-filled) num-entries))))
          (plz-run metadata-queue)
          (display-buffer (current-buffer) hyperdrive-directory-display-buffer-action)
          ;; TODO: Should we display the buffer before or after calling THEN? (test with yank-media handler)
          (when then
            (funcall then)))))))

(defun hyperdrive-column-headers (prefix)
  "Return column headers as a string with PREFIX.
Columns are suffixed with up/down arrows according to
`hyperdrive-sort-entries'."
  (let (name-arrow size-arrow date-arrow)
    (pcase-exhaustive hyperdrive-directory-sort
      (`(hyperdrive-entry-name . ,predicate)
       (setf name-arrow (pcase-exhaustive predicate
                          ('string< "▲")
                          ('string> "▼"))))
      (`(hyperdrive-entry-size . ,predicate)
       (setf size-arrow (pcase-exhaustive predicate
                          ('< "▲")
                          ('> "▼"))))
      (`(hyperdrive-entry-modified . ,predicate)
       (setf date-arrow (pcase-exhaustive predicate
                          ('time-less-p "▲")
                          ((pred functionp) "▼")))))
    (concat prefix "\n"
            (format "%6s  %s  %s"
                    (concat size-arrow
                            (propertize "Size" 'face 'hyperdrive-column-header))
                    (format hyperdrive-timestamp-format-string
			    (concat date-arrow
				    (propertize "Last Modified" 'face 'hyperdrive-column-header)))
                    (concat (propertize "Name" 'face 'hyperdrive-column-header)
                            name-arrow)))))

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

(declare-function hyperdrive-mode "hyperdrive")
(cl-defun hyperdrive-handler-html (entry &key then)
  "Show ENTRY, where ENTRY is an HTML file.
Renders HTML with `shr-insert-document', then calls THEN if
given."
  (if hyperdrive-render-html
      (progn
        (eww (hyperdrive-entry-url entry))
        ;; Set `hyperdrive-current-entry' and use `hyperdrive-mode'
        ;; for remapped keybindings for, e.g., `hyperdrive-up'.
        (setq-local hyperdrive-current-entry entry)
        (hyperdrive-mode)
        (when then
          (funcall then)))
    (hyperdrive-handler-default entry :then then)))

(cl-defun hyperdrive-handler-image (entry &key then)
  "Show ENTRY, where ENTRY is an image file.
Then calls THEN if given."
  (hyperdrive-handler-default
   entry :then (lambda ()
		 (image-mode)
		 (when then
		   (funcall then)))))

(defun hyperdrive-url-loader (parsed-url)
  "Retrieve URL synchronously.
PARSED-URL must be a URL-struct like the output of
`url-generic-parse-url'.

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
