;;; hyperdrive-history.el --- Hyperdrive version history frontend  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024  USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>

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

;;;; Variables

(defvar-local h/history-current-entry nil
  ;; NOTE: We don't reuse `hyperdrive-current-entry' in history buffers because
  ;; functions like `hyperdrive--find-buffer-visiting' expect only one buffer to
  ;; be visiting an entry at a time.
  "Entry for current history buffer.")

;;;; Functions

(cl-defun h/history-url (entry &optional (start "") (end ""))
  "Return formatted history URL for ENTRY, START, and END."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
               ((cl-struct hyperdrive public-key latest-version) hyperdrive))
    (format "hyper://%s/$/history/%s..%s%s" public-key start end path)))

(defcustom h/history-load-limit 100
  "Number of entry versions to load at once.
Affects `hyperdrive-history-load', which see.")

(cl-defun h/history-load (entry &key start end then)
  "Load file history for ENTRY then call THEN with no arguments.
START and END may be numbers specifying the history range to get
from the gateway.  If START and/or END are omitted, the gateway
will implicitly guess the range."
  (declare (indent defun))
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
               ((cl-struct hyperdrive public-key latest-version) hyperdrive)
               (earliest-start (- (or end latest-version) h/history-load-limit))
               (start (if (>= start earliest-start) start earliest-start)))
    (h/api 'get (h/history-url entry start end) :as 'buffer
      :headers `(("X-History-Load-Only" . t))
      :else (lambda (err)
              (h/error "Unable to get history for `%s': %S" (he/url entry) err))
      :then (lambda (_buffer)
              ;; TODO: Get latest-version from gateway and persist drive
              (funcall then)))))

(cl-defun h/history-get (entry &key start end)
  "Return list of file history entries for ENTRY.
START and END may be numbers specifying the history range to get
from the gateway.  If START and/or END are omitted, the gateway
will implicitly guess the range."
  (declare (indent defun))
  (pcase-let*
      (((cl-struct hyperdrive-entry hyperdrive path) entry)
       ((cl-struct hyperdrive public-key latest-version) hyperdrive)
       ((cl-struct plz-response body headers)
        (condition-case err
            (h/api 'get (h/history-url entry start end) :as 'response)
          (err (h/error "Unable to get history for `%s': %S" (he/url entry)
                        err))))
       (history-entries))
    (pcase-dolist ((map exists blockLengthDownloaded version value)
                   (json-parse-string
                    body :object-type 'alist :array-type 'list
                    :false-object nil :null-object nil))
      (let ((history-entry (he/create :hyperdrive hyperdrive
                                      :path path :version version)))
        (setf (he/size history-entry)
              (map-elt (map-elt value 'blob) 'byteLength))
        (setf (map-elt (he/etc history-entry) 'block-length)
              (map-elt (map-elt value 'blob) 'blockLength))
        (setf (map-elt (he/etc history-entry) 'block-length-downloaded)
              blockLengthDownloaded)
        (when-let ((mtime
                    (map-elt (map-elt value 'metadata) 'mtime)))
          (setf (he/mtime history-entry)
                (seconds-to-time (/ mtime 1000.0))))
        (setf (map-elt (he/etc history-entry) 'range-end)
              (if-let ((next-entry (car history-entries)))
                  (1- (he/version next-entry))
                latest-version))
        (setf (map-elt (he/etc history-entry) 'existsp)
              (pcase exists
                ("unknown" 'unknown)
                (_ exists)))
        (push history-entry history-entries)))
    ;; TODO: Get latest-version from gateway and persist drive
    history-entries))

(defun h/history-find-buffer-visiting (entry)
  "Return a buffer showing ENTRY's history, or nil if none exists."
  ;; There should only ever be one buffer showing ENTRY's history, so it's safe
  ;; to return the first value in the list.
  (car (match-buffers
        (lambda (buffer)
          (and-let* ((local-entry (buffer-local-value 'h/history-current-entry buffer)))
            (he/equal-p entry local-entry t))))))

(defun h/history-find-at-point (event)
  "Find entry at EVENT's position."
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'h/history-find-file-other-window))

(defun h/history-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  ;; FIXME: Perform type-checking? If not, is this function necessary?
  (insert (h/history--format-entry thing)))

(defun h/history--format-entry (entry)
  "Return ENTRY formatted as a string.
ENTRY's ETC slot must have `existsp' and `range-end' keys."
  (pcase-let*
      (((cl-struct hyperdrive-entry version size mtime etc) entry)
       ((map block-length block-length-downloaded existsp range-end) etc)
       (formatted-range (if (eq version range-end)
                            (format "%d" version)
                          (format "%d-%d" version range-end)))
       (exists-marker (format "%7s" (pcase-exhaustive existsp
                                      ('t "Yes")
                                      ('nil "No")
                                      ('unknown "Unknown"))))
       (size (and size (file-size-human-readable size)))
       (timestamp (if mtime
                      (format-time-string h/timestamp-format mtime)
                    (propertize " " 'display '(space :width h/timestamp-width)))))
    ;; FIXME: Use dynamic width of range column equal to 2N+1, where N
    ;; is the width of the hyperdrive's latest version
    (format
     "%7s  %19s  %6s  %s"
     (propertize exists-marker
                 'face (pcase-exhaustive existsp
                         ('t 'h/history-existent)
                         ('nil 'h/history-nonexistent)
                         ('unknown 'h/history-unknown)))
     (propertize formatted-range
                 'face 'h/history-range
                 'mouse-face 'highlight
                 'help-echo (format (pcase-exhaustive existsp
                                      ('t "Open version %s")
                                      ('nil "Nonexistent at version %s")
                                      ('unknown "Load history at version %s"))
                                    version))
     (propertize (or size "")
                 'face (and block-length-downloaded block-length
                            (pcase block-length-downloaded
                              (0 'h/size-not-downloaded)
                              ((pred (= block-length)) 'h/size-fully-downloaded)
                              (_ 'h/size-partially-downloaded)))
                 'help-echo (format "%s of %s blocks downloaded"
                                    block-length-downloaded block-length))
     (propertize (or timestamp "")
                 'face 'h/timestamp))))

(defun h/history-entry-at-point ()
  "Return entry at point.
With point below last entry, signals a user-error.  With point on
header, returns an entry whose RANGE-END and version are nil."
  (let ((current-line (line-number-at-pos))
        (last-line (line-number-at-pos (ewoc-location (ewoc-nth h/ewoc -1))))
        (entry-at-point (ewoc-data (ewoc-locate h/ewoc))))
    (cond ((= 1 current-line) ; Point on header: return version-less entry
           (let ((copy-entry (compat-call copy-tree entry-at-point t)))
             (setf (map-elt (he/etc copy-entry) 'range-end) nil)
             (setf (he/version copy-entry) nil)
             copy-entry))
          ((or (> current-line last-line) (= 2 current-line))
           ;; Point is below the last entry or on column headers: signal error.
           (h/user-error "No file on this line"))
          (t ; Point on a file entry: return its entry.
           entry-at-point))))

(defun h/history-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `hyperdrive-history-mode' buffer."
  ;; TODO: Preserve point position in buffer.
  (h/history h/history-current-entry))

(defun hyperdrive-history--invalidate-entry (entry)
  "Invalidate ENTRY's ewoc node in history buffers.
Invalidated ewoc node entries will have these slots updated:

- ETC
  + BLOCK-LENGTH-DOWNLOADED

All other slots in each ewoc node entry data will be reused."
  (when-let* ((history-buffer (h/history-find-buffer-visiting entry))
              (history-ewoc (buffer-local-value 'h/ewoc history-buffer))
              (history-node (h/ewoc-find-node history-ewoc entry
                              :predicate #'he/within-version-range-p))
              (history-ewoc-entry (ewoc-data history-node)))
    (setf (map-elt (he/etc history-ewoc-entry) 'block-length-downloaded)
          (map-elt (he/etc entry) 'block-length-downloaded))
    (ewoc-set-data history-node history-ewoc-entry)
    ;; NOTE: Ensure that the buffer's window is selected,
    ;; if it has one.  (Workaround a possible bug in EWOC.)
    (if-let ((buffer-window (get-buffer-window history-buffer)))
        (with-selected-window buffer-window
          (with-silent-modifications (ewoc-invalidate history-ewoc history-node)))
      (with-current-buffer history-buffer
        (with-silent-modifications (ewoc-invalidate history-ewoc history-node))))))

;;;; Mode

(defvar-keymap h/history-mode-map
  :parent  h/ewoc-mode-map
  :doc "Local keymap for `hyperdrive-history-mode' buffers."
  "RET" #'h/history-find-file
  "o"   #'h/history-find-file-other-window
  "v"   #'h/history-view-file
  "="   #'h/history-diff
  "+"   #'h/history-fill-version-ranges
  "w"   #'h/history-copy-url
  "d"   #'h/history-download-file
  "F"   #'h/history-forget-file
  "<mouse-2>" #'h/history-find-at-point
  "<follow-link>" 'mouse-face)

(define-derived-mode h/history-mode h/ewoc-mode
  `("Hyperdrive-history"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive history buffers."
  ;; TODO: Consider keeping the version around so that we can highlight the line
  ;; corresponding to version currently open in another buffer.
  :group 'hyperdrive
  :interactive nil
  (setf h/ewoc (ewoc-create #'h/history-pp))
  (setq-local revert-buffer-function #'h/history-revert-buffer))

;;;; Commands

;;;###autoload
(defun hyperdrive-history (entry)
  "Display version history for current hyperdrive ENTRY.

Interactively, open version history for current file ENTRY or
ENTRY at point in a directory.  Otherwise, or with universal
prefix argument \\[universal-argument], prompt for ENTRY."
  (interactive (list (h//context-entry :force-prompt current-prefix-arg)))
  ;; TODO: Highlight range for ENTRY
  (when (h//entry-directory-p entry)
    (h/user-error "Directory history not implemented"))
  (pcase-let*
      (((cl-struct hyperdrive-entry hyperdrive path) entry)
       (main-header (h//format-entry entry "[%H] %p"))
       (header (format
                "%s\n%7s  %19s  %6s  %s" main-header
                (propertize "Exists" 'face 'h/column-header)
                (propertize "Drive Version Range" 'face 'h/column-header)
                (propertize "Size" 'face 'h/column-header)
                (string-pad (propertize "Last Modified" 'face 'h/column-header)
                            h/timestamp-width nil t)))
       (queue) (ewoc))
    (with-current-buffer (get-buffer-create
                          (format "*Hyperdrive-history: %s*"
                                  (h//format-entry entry "[%H] %p")))
      (with-silent-modifications
        (h/history-mode)
        (setq-local h/history-current-entry entry)
        (setf ewoc h/ewoc) ; Bind this for the he/fill lambda.
        (ewoc-filter h/ewoc #'ignore)
        (erase-buffer))
      ;; TODO: Display files in pop-up window, like magit-diff buffers appear when selected from magit-log
      (display-buffer (current-buffer) h/history-display-buffer-action)
      (dolist (history-entry (h/history-get entry))
        (ewoc-enter-first ewoc history-entry))
      (ewoc-set-hf ewoc header "")
      (with-silent-modifications (ewoc-refresh h/ewoc))
      (goto-char (point-min)))))

(defun h/history-fill-version-ranges (entry)
  "Fill version ranges starting from ENTRY at point."
  (interactive (list (h/history-entry-at-point)))
  (pcase-let
      (((cl-struct h/entry version (etc (map range-end))) entry)
       (current-entry h/history-current-entry))
    (overlay-put (make-overlay (pos-bol) (+ (pos-bol) (length "Loading")))
                 'display "Loading")
    (h/history-load entry :start version :end range-end
      :then (lambda () (h/history current-entry)))))

(declare-function h/diff-file-entries "hyperdrive-diff")
(defun h/history-diff (old-entry new-entry)
  "Show diff between OLD-ENTRY and NEW-ENTRY.
Interactively, diff entry at point with previous entry."
  ;; TODO: Set entries based on marked ranges
  ;; TODO: What to do for unknown-existent entries?
  (interactive (let* ((new-entry (h/history-entry-at-point))
                      (old-entry (he/previous new-entry)))
                 (unless old-entry
                   (setf old-entry (compat-call copy-tree new-entry t))
                   (cl-decf (he/version old-entry)))
                 (list old-entry new-entry)) h/history-mode)
  (h/diff-file-entries old-entry new-entry
    :then (lambda ()
            (pop-to-buffer (current-buffer)))))

(defun h/history-find-file (entry)
  "Visit hyperdrive ENTRY at point.
When entry does not exist, does nothing and returns nil.  When
entry is not known to exist, attempts to load entry at ENTRY's
ETC slot's RANGE-END value.

Interactively, visit entry at point in `hyperdrive-history'
buffer."
  (interactive (list (h/history-entry-at-point)) h/history-mode)
  (pcase-exhaustive (map-elt (he/etc entry) 'existsp)
    ('t (h/open entry))
    ('nil (h/user-error "File does not exist!"))
    ('unknown (h/history-fill-version-ranges entry))))

(defun h/history-find-file-other-window (entry)
  "Visit hyperdrive ENTRY at point in other window.
Then call THEN.  When entry does not exist, does nothing and
returns nil.  When entry is not known to exist, attempts to load
entry at ENTRY's ETC slot's RANGE-END value.

Interactively, visit entry at point in `hyperdrive-history'
buffer."
  (interactive (list (h/history-entry-at-point)) h/history-mode)
  (h/open entry :then (lambda () (pop-to-buffer (current-buffer) t))))

(declare-function h/view-file "hyperdrive")
(defun h/history-view-file (entry)
  "Open hyperdrive ENTRY at point in `view-mode'.
When entry does not exist or is not known to exist, does nothing
and returns nil.

Interactively, visit entry at point in `hyperdrive-history'
buffer."
  (interactive (list (h/history-entry-at-point)) h/history-mode)
  (pcase-exhaustive (map-elt (he/etc entry) 'existsp)
    ('t (h/view-file entry))
    ('nil (h/user-error "File does not exist!"))
    ('unknown (h/history-fill-version-ranges entry))))

(declare-function h/copy-url "hyperdrive")
(defun h/history-copy-url (entry)
  "Copy URL of ENTRY into the kill ring."
  (interactive (list (h/history-entry-at-point)) h/history-mode)
  (pcase-exhaustive (map-elt (he/etc entry) 'existsp)
    ('t (h/copy-url entry))
    ('nil (h/user-error "File does not exist!"))
    ('unknown (h/user-error "File not known to exist!"))))

(declare-function h/download "hyperdrive")
(defun h/history-download-file (entry filename)
  "Download ENTRY at point to FILENAME on disk."
  (interactive
   (pcase-let*
       ((entry (h/history-entry-at-point))
        ((cl-struct hyperdrive-entry name) entry)
        (read-filename (and (eq t (map-elt (he/etc entry) 'existsp))
                            ;; Only prompt for filename when entry exists

                            ;; FIXME: This function is only intended for
                            ;; interactive use. Is it acceptable to have a nil
                            ;; argument list and perform the user interactions
                            ;; in the body? This change would deduplicate the
                            ;; check for the existence of the entry.
                            (read-file-name
                             "Filename: "
                             (expand-file-name name h/download-directory)))))
     (list entry read-filename)) h/history-mode)
  (pcase-exhaustive (map-elt (he/etc entry) 'existsp)
    ('t (h/download entry filename))
    ('nil (h/user-error "File does not exist!"))
    ('unknown (h/user-error "File not known to exist!"))))

(declare-function h/forget-file "hyperdrive")
(defun h/history-forget-file (entry)
  "Delete local copy of the file contents of ENTRY at point.
Only delete the blob(s) for the file at ENTRY's version range;
other blobs are not cleared.  Hyperdrive directory contents are
not modified; file blobs may be recoverable from other peers."
  (interactive (list (h/history-entry-at-point)) h/history-mode)
  (h/forget-file entry))

(provide 'hyperdrive-history)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive-history.el ends here
