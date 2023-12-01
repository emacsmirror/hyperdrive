;;; hyperdrive-history.el --- Hyperdrive version history frontend  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USHIN, Inc.

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
(require 'h/ewoc)

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
  (insert (h/history--format-range-entry thing)))

(defun h/history--format-range-entry (range-entry)
  "Return RANGE-ENTRY formatted as a string.
RANGE-ENTRY is a cons cell whose car is a range according to
`hyperdrive-version-ranges', except that \\+`:existsp' may have the
value \\+`unknown', and whose cdr is a hyperdrive entry."
  (pcase-let*
      ((`(,range . ,entry) range-entry)
       (`(,range-start . ,(map (:range-end range-end) (:existsp existsp))) range)
       ((cl-struct hyperdrive-entry size mtime) entry)
       (formatted-range (if (eq range-start range-end)
                            (format "%d" range-start)
                          (format "%d-%d" range-start range-end)))
       (exists-marker (format "%7s" (pcase-exhaustive existsp
                                      ('t "Yes")
                                      ('nil "No")
                                      ('unknown "Unknown"))))
       (size (when size
               (file-size-human-readable size)))
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
                                    range-start))
     (propertize (or size "")
                 'face 'h/size)
     (propertize (or timestamp "")
                 'face 'h/timestamp))))

(defun h/history-range-entry-at-point ()
  "Return range-entry at version at point.
With point below last entry, signals a user-error.
With point on header, returns a rangle-entry whose RANGE-END
and ENTRY's version are nil."
  (let ((current-line (line-number-at-pos))
        (last-line (line-number-at-pos (ewoc-location (ewoc-nth h/ewoc -1))))
        (range-entry-at-point (ewoc-data (ewoc-locate h/ewoc))))
    (cond ((= 1 current-line)
           ;; Point on header: set range-end and entry version to nil
           (pcase-let ((`(,range . ,entry)
                        (h/copy-tree range-entry-at-point t)))
             (setf (map-elt (cdr range) :range-end) nil)
             (setf (he/version entry) nil)
             (cons range entry)))
          ((or (> current-line last-line) (= 2 current-line))
           ;; Point is below the last entry or on column headers: signal error.
           (h/user-error "No file on this line"))
          (t
           ;; Point on a file entry: return its entry.
           range-entry-at-point))))

(defun h/range-entry-exists-p (range-entry)
  "Return status of RANGE-ENTRY's existence at its version.

- t       :: ENTRY is known to exist.
- nil     :: ENTRY is known to not exist.
- unknown :: ENTRY is not known to exist."
  (pcase-let* ((range (car range-entry))
               ((map (:existsp existsp)) (cdr range)))
    existsp))

(defun h/history-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `hyperdrive-history-mode' buffer."
  ;; TODO: Preserve point position in buffer.
  (h/history h/current-entry))

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
  "<mouse-2>" #'h/history-find-at-point
  "<follow-link>" 'mouse-face)

(define-derived-mode h/history-mode h/ewoc-mode
  `("Hyperdrive-history"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive history buffers."
  ;; TODO: Add revert buffer function. This will likely require
  ;; binding h/current-entry in this mode. Consider keeping
  ;; the version around so that we can highlight the line
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
       (range-entries
        (mapcar (lambda (range)
                  ;; Some entries may not exist at `range-start',
                  ;; as in the version before it was created, see:
                  ;; (info "(hyperdrive)Versioning")
                  (cons range
                        (he/create
                         :hyperdrive hyperdrive
                         :path path
                         ;; Set version to range-start
                         :version (car range))))
                ;; Display in reverse chronological order
                (nreverse (he/version-ranges-no-gaps entry))))
       (main-header (h//format-entry entry "[%H] %p"))
       (header (concat main-header "\n"
                       (format "%7s  %19s  %6s  %s"
                               (propertize "Exists" 'face 'h/column-header)
                               (propertize "Drive Version Range" 'face 'h/column-header)
                               (propertize "Size" 'face 'h/column-header)
                               (format (format "%%%ds" h/timestamp-width)
                                       (propertize "Last Modified" 'face 'h/column-header)))))
       (queue) (ewoc))
    (with-current-buffer (get-buffer-create
                          (format "*Hyperdrive-history: %s*"
                                  (h//format-entry entry "[%H] %p")))
      (with-silent-modifications
        (h/history-mode)
        (setq-local h/current-entry entry)
        (setf ewoc h/ewoc) ; Bind this for the h/fill lambda.
        (ewoc-filter h/ewoc #'ignore)
        (erase-buffer)
        (ewoc-set-hf h/ewoc header "")
        (mapc (lambda (range-entry)
                (ewoc-enter-last h/ewoc range-entry))
              range-entries))
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
      (mapc (lambda (range-entry)
              (when (eq t (h/range-entry-exists-p range-entry))
                ;; TODO: Handle failures?
                (h/fill (cdr range-entry) :queue queue :then #'ignore)))
            range-entries)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

;; TODO: Add pcase-defmacro for destructuring range-entry
(defun h/history-fill-version-ranges (range-entry)
  "Fill version ranges starting from RANGE-ENTRY at point."
  (interactive (list (h/history-range-entry-at-point)))
  (pcase-let* ((`(,range . ,entry) range-entry)
               (`(,_range-start . ,(map (:range-end range-end))) range)
               (range-end-entry (h/copy-tree entry))
               (ov (make-overlay (pos-bol) (+ (pos-bol) (length "Loading")))))
    (setf (he/version range-end-entry) range-end)
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
Interactively, diff range entry at point with previous entry."
  ;; TODO: Set entries based on marked ranges
  ;; TODO: What to do for unknown range-entries?
  (interactive (let* ((new-entry (cdr (h/history-range-entry-at-point)))
                      (old-entry (he/previous new-entry)))
                 (unless old-entry
                   (setf old-entry (h/copy-tree new-entry t))
                   (cl-decf (he/version old-entry)))
                 (list old-entry new-entry)) h/history-mode)
  (h/diff-file-entries old-entry new-entry
    :then (lambda ()
            (pop-to-buffer (current-buffer)))))

(cl-defun h/history-find-file
    (range-entry &key (then (lambda ()
                              (pop-to-buffer (current-buffer)
                                             '(display-buffer-same-window)))))
  "Visit hyperdrive entry in RANGE-ENTRY at point.
Then call THEN.  When entry does not exist, does nothing and
returns nil.  When entry is not known to exist, attempts to load
entry at RANGE-ENTRY's RANGE-END.

Interactively, visit entry at point in `hyperdrive-history'
buffer."
  (interactive (list (h/history-range-entry-at-point)) h/history-mode)
  (pcase-exhaustive (h/range-entry-exists-p range-entry)
    ('t
     ;; Known to exist: open it.
     (h/open (cdr range-entry) :then then))
    ('nil
     ;; Known to not exist: warn user.
     (h/user-error "File does not exist!"))
    ('unknown
     ;; Not known to exist: fill version ranges:
     (h/history-fill-version-ranges range-entry))))

(defun h/history-find-file-other-window (range-entry)
  "Visit hyperdrive entry in RANGE-ENTRY at point in other window.
Then call THEN.  When entry does not exist, does nothing and
returns nil.  When entry is not known to exist, attempts to load
entry at RANGE-ENTRY's RANGE-END.

Interactively, visit entry at point in `hyperdrive-history'
buffer."
  (interactive (list (h/history-range-entry-at-point)) h/history-mode)
  (h/history-find-file
   range-entry :then (lambda ()
                       (pop-to-buffer (current-buffer) t))))

(declare-function h/view-file "hyperdrive")
(defun h/history-view-file (range-entry)
  "Open hyperdrive entry in RANGE-ENTRY at point in `view-mode'.
When entry does not exist or is not known to exist, does nothing
and returns nil.

Interactively, visit entry at point in `hyperdrive-history'
buffer."
  (interactive (list (h/history-range-entry-at-point)) h/history-mode)
  (pcase-exhaustive (h/range-entry-exists-p range-entry)
    ('t
     ;; Known to exist: open it.
     (h/view-file (cdr range-entry)))
    ('nil
     ;; Known to not exist: warn user.
     (h/user-error "File does not exist!"))
    ('unknown
     ;; Not known to exist: fill version ranges:
     (h/history-fill-version-ranges range-entry))))

(declare-function h/copy-url "hyperdrive")

(defun h/history-copy-url (range-entry)
  "Copy URL of entry in RANGE-ENTRY into the kill ring."
  (interactive (list (h/history-range-entry-at-point)) h/history-mode)
  (pcase-exhaustive (h/range-entry-exists-p range-entry)
    ('t
     ;; Known to exist: copy it.
     (h/copy-url (cdr range-entry)))
    ('nil
     ;; Known to not exist: warn user.
     (h/user-error "File does not exist!"))
    ('unknown
     ;; Not known to exist: warn user.
     (h/user-error "File not known to exist!"))))

(declare-function h/download "hyperdrive")

(defun h/history-download-file (range-entry filename)
  "Download entry in RANGE-ENTRY at point to FILENAME on disk."
  (interactive
   (pcase-let* ((range-entry (h/history-range-entry-at-point))
                ((cl-struct hyperdrive-entry name) (cdr range-entry))
                (read-filename (when (eq t (h/range-entry-exists-p range-entry))
                                 ;; Only prompt for filename when entry exists

                                 ;; FIXME: This function is only intended for
                                 ;; interactive use. Is it acceptable to have a nil
                                 ;; argument list and perform the user interactions
                                 ;; in the body? This change would deduplicate the
                                 ;; check for the existence of the entry.
                                 (read-file-name
                                  "Filename: "
                                  (expand-file-name name h/download-directory)))))
     (list range-entry read-filename)) h/history-mode)
  (pcase-exhaustive (h/range-entry-exists-p range-entry)
    ('t
     ;; Known to exist: download it.
     (h/download (cdr range-entry) filename))
    ('nil
     ;; Known to not exist: warn user.
     (h/user-error "File does not exist!"))
    ('unknown
     ;; Not known to exist: warn user.
     (h/user-error "File not known to exist!"))))

(provide 'h/history)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive-history.el ends here
