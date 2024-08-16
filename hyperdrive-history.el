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

;;;; Functions

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
  (interactive (list (h//context-entry)))
  ;; TODO: Highlight range for ENTRY
  (when (h//entry-directory-p entry)
    (h/user-error "Directory history not implemented"))
  (pcase-let*
      (((cl-struct hyperdrive-entry hyperdrive path) entry)
       (entries
        ;; TODO: Double check and continue.
        (mapcar (pcase-lambda (`(,range-start . ,(map :range-end :existsp)))
                  ;; Some entries may not exist at `range-start',
                  ;; as in the version before it was created, see:
                  ;; (info "(hyperdrive)Versioning")
                  (he/create
                   :hyperdrive hyperdrive
                   :path path
                   ;; Set version to range-start
                   :version range-start
                   :etc `((existsp . ,existsp) (range-end . ,range-end))))
                ;; Display in reverse chronological order
                (nreverse (he/version-ranges-no-gaps entry))))
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
        (erase-buffer)
        (ewoc-set-hf h/ewoc header "")
        (mapc (apply-partially #'ewoc-enter-last h/ewoc) entries))
      ;; TODO: Display files in pop-up window, like magit-diff buffers appear when selected from magit-log
      (display-buffer (current-buffer) h/history-display-buffer-action)
      (setf queue
            (make-plz-queue
             :limit h/queue-limit
             :finally (lambda ()
                        ;; NOTE: Ensure that the buffer's window is selected,
                        ;; if it has one.  (Workaround a possible bug in EWOC.)
                        (if-let ((buffer-window (get-buffer-window (ewoc-buffer ewoc))))
                            (with-selected-window buffer-window
                              ;; TODO: Use `ewoc-invalidate' on individual entries
                              ;; (maybe later, as performance comes to matter more).
                              (with-silent-modifications (ewoc-refresh h/ewoc))
                              (goto-char (point-min)))
                          (with-current-buffer (ewoc-buffer ewoc)
                            (with-silent-modifications (ewoc-refresh h/ewoc))
                            (goto-char (point-min))))
                        ;; TODO: Accept then argument?
                        ;; (with-current-buffer (ewoc-buffer ewoc)
                        ;;   (when then
                        ;;     (funcall then)))
                        )))
      (dolist (entry entries)
        (when (eq t (map-elt (he/etc entry) 'existsp))
          ;; TODO: Handle failures?
          (he/fill entry :queue queue :then #'ignore)))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

(defun h/history-fill-version-ranges (entry)
  "Fill version ranges starting from ENTRY at point."
  (interactive (list (h/history-entry-at-point)))
  (pcase-let* (((cl-struct h/entry hyperdrive etc) entry)
               ((cl-struct hyperdrive latest-version) hyperdrive)
               ((map range-end) etc)
               (range-end-entry (compat-call copy-tree entry t))
               (ov (make-overlay (pos-bol) (+ (pos-bol) (length "Loading")))))
    (setf (he/version range-end-entry) (or range-end latest-version))
    (overlay-put ov 'display "Loading")
    (h/fill-version-ranges range-end-entry
      :finally (lambda ()
                 ;; TODO: Should we open the history buffer for entry
                 ;; or range-end-entry or...?
                 (delete-overlay ov)
                 (h/history entry)))))

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
