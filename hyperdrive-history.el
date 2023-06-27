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
(require 'hyperdrive-ewoc)

;;;; Functions

(defun hyperdrive-history-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  ;; FIXME: Perform type-checking? If not, is this function necessary?
  (insert (hyperdrive-history--format-range-entry thing)))

(defun hyperdrive-history--format-range-entry (range-entry)
  "Return RANGE-ENTRY formatted as a string.
RANGE-ENTRY is a cons cell whose car is a range according to
`hyperdrive-version-ranges', except that :EXISTSP may have the
value 'UNKNOWN.

and whose cdr is a hyperdrive entry."
  (pcase-let* ((`(,range . ,entry) range-entry)
               (`(,range-start . ,(map (:range-end range-end) (:existsp existsp))) range)
               ((cl-struct hyperdrive-entry size modified) entry)
               (formatted-range (if (eq range-start range-end)
                                    (format "%d" range-start)
                                  (format "%d-%d" range-start range-end)))
               (exists-marker (pcase-exhaustive existsp
                                ('t "Y")
                                ('nil "X")
                                ('unknown "?")))
               (size (when size
                       (file-size-human-readable size)))
               (timestamp (if modified
                              (format-time-string hyperdrive-timestamp-format modified)
                            (format hyperdrive-timestamp-format-string " "))))
    ;; FIXME: Use dynamic width of range column equal to 2N+1, where N
    ;; is the width of the hyperdrive's latest version
    (format "%s  %10s  %6s  %s"
            (propertize exists-marker
                        'face (pcase-exhaustive existsp
                                ('t 'hyperdrive-history-existent)
                                ('nil 'hyperdrive-history-nonexistent)
                                ('unknown 'hyperdrive-history-unknown)))
            (propertize formatted-range
                        'face 'hyperdrive-history-range)
            (propertize (or size "")
                        'face 'hyperdrive-size)
            (propertize (pcase-exhaustive existsp
                          ('t (or timestamp ""))
                          ('nil "nonexistent")
                          ('unknown "unknown"))
                        ;; TODO: Consider adding separate faces for "nonexistent" and "unknown" text
                        'face 'hyperdrive-timestamp))))

(defun hyperdrive-history-range-entry-at-point ()
  "Return range-entry at version at point.
With point below last entry, returns nil.
With point on header, returns a rangle-entry whose RANGE-END
and ENTRY's version are nil."
  (let ((current-line (line-number-at-pos))
        (last-line (line-number-at-pos (ewoc-location (ewoc-nth hyperdrive-ewoc -1))))
        (current-range-entry (ewoc-data (ewoc-locate hyperdrive-ewoc))))
    (cond ((= 1 current-line)
           ;; Point on header: set range-end and entry version to nil
           (pcase-let ((`(,range . ,entry)
                        (hyperdrive-copy-tree current-range-entry t)))
             (setf (map-elt (cdr range) :range-end) nil)
             (setf (hyperdrive-entry-version entry) nil)
             (cons range entry)))
          ((> current-line last-line)
           ;; Point is below the last entry: return nil.
           nil)
          (t
           ;; Point on a file entry: return its entry.
           current-range-entry))))

(defun hyperdrive-range-entry-exists-p (range-entry)
  "Return status of ENTRY-RANGE's existence at its version.

- t       :: ENTRY is known to exist.
- nil     :: ENTRY is known to not exist.
- unknown :: ENTRY is not known to exist."
  (pcase-let* ((range (car range-entry))
               ((map (:existsp existsp)) (cdr range)))
    existsp))

;;;; Mode

(defvar-keymap hyperdrive-history-mode-map
  :parent  hyperdrive-ewoc-mode-map
  :doc "Local keymap for `hyperdrive-history-mode' buffers."
  "RET" #'hyperdrive-history-find-file
  "w"   #'hyperdrive-history-copy-url
  "d"   #'hyperdrive-history-download-file)

(define-derived-mode hyperdrive-history-mode hyperdrive-ewoc-mode
  `("Hyperdrive-history"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive history buffers."
  ;; TODO: Add revert buffer function. This will likely require
  ;; binding hyperdrive-current-entry in this mode. Consider keeping
  ;; the version around so that we can highlight the line
  ;; corresponding to version currently open in another buffer.
  :interactive nil
  (setf hyperdrive-ewoc (ewoc-create #'hyperdrive-history-pp)))

;;;; Commands


;; TODO: Test with deleted and recreated files

;;;###autoload
(defun hyperdrive-history (entry)
  "Display version history for current hyperdrive ENTRY.

Prefix argument forces `hyperdrive-read-entry' to prompt for an
entry."
  ;; TODO: Traverse history backward from known existent ranges.
  (interactive (list (if (or current-prefix-arg (not hyperdrive-current-entry))
                         (hyperdrive-read-entry :force-prompt t :allow-version-p nil)
                       hyperdrive-current-entry)))
  ;; TODO: Highlight range for ENTRY
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
               (range-entries
                (mapcar (lambda (range)
                          ;; Some entries may not exist at
                          ;; `range-start', as in the version before
                          ;; it was created. See manual:
                          ;; [[info:hyperdrive-manual.info#Versioning]]
                          (cons range
                                (hyperdrive-entry-create
                                 :hyperdrive hyperdrive
                                 :path path
                                 ;; Set version to range-start
                                 :version (car range))))
                        ;; Display in reverse chronological order
                        (nreverse (hyperdrive-entry-version-ranges-no-gaps entry))))
               (header (hyperdrive-entry-description
                        ;; Pass entry without version to
                        ;; `hyperdrive-entry-description' so header has no version.
                        (hyperdrive-entry-create :hyperdrive hyperdrive :path path)))
               (inhibit-read-only t)
               (queue) (ewoc))
    (with-current-buffer (get-buffer-create
                          (format "*Hyperdrive-history: %s %s*"
                                  (hyperdrive--format-host hyperdrive :format hyperdrive-default-host-format
                                                           :with-label t)
                                  (url-unhex-string path)))
      (hyperdrive-history-mode)
      (setf ewoc hyperdrive-ewoc) ; Bind this for the hyperdrive-fill lambda.
      (ewoc-filter hyperdrive-ewoc #'ignore)
      (erase-buffer)
      ;; TODO: Display hyperdrive's latest version, maybe in a footer?
      ;; (footer (number-to-string (hyperdrive-latest-version hyperdrive)))
      (ewoc-set-hf hyperdrive-ewoc header "")
      (mapc (lambda (range-entry)
              (ewoc-enter-last hyperdrive-ewoc range-entry))
            range-entries)
      ;; TODO: Display files in pop-up window, like magit-diff buffers appear when selected from magit-log
      (display-buffer (current-buffer) hyperdrive-history-display-buffer-action)
      (setf queue (make-plz-queue :limit 8
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
                                             ;; TODO: Accept then argument?
                                             ;; (with-current-buffer (ewoc-buffer ewoc)
                                             ;;   (when then
                                             ;;     (funcall then)))
                                             )))
      (mapc (lambda (range-entry)
              (when (eq t (hyperdrive-range-entry-exists-p range-entry))
                ;; TODO: Handle failures?
                (hyperdrive-fill (cdr range-entry) :queue queue :then #'ignore)))
            range-entries)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))


(declare-function hyperdrive-open "hyperdrive")

(defun hyperdrive-history-find-file (range-entry)
  "Visit hyperdrive entry in RANGE-ENTRY at point.
When entry does not exist, does nothing and returns nil.
When entry is not known to exist, attempts to load entry at
RANGE-ENTRY's RANGE-END.

Interactively, visit entry at point in `hyperdrive-history'
buffer."
  (declare (modes hyperdrive-history-mode))
  (interactive (list (hyperdrive-history-range-entry-at-point)))
  (cl-ecase (hyperdrive-range-entry-exists-p range-entry)
    ((t quote)
     ;; Known to exist: open it.
     (hyperdrive-open (cdr range-entry)))
    ((nil quote)
     ;; Known to not exist: warn user.
     (user-error "File does not exist!"))
    ((unknown quote)
     ;; Not known to exist: prompt user
     ;; TODO: Design options
     (hyperdrive-message "File not known to exist. What do you want to do?"))))

(declare-function hyperdrive-copy-url "hyperdrive")

(defun hyperdrive-history-copy-url (range-entry)
  "Copy URL of entry in RANGE-ENTRY into the kill ring."
  (declare (modes hyperdrive-history-mode))
  (interactive (list (hyperdrive-history-range-entry-at-point)))
  (cl-ecase (hyperdrive-range-entry-exists-p range-entry)
    ((t quote)
     ;; Known to exist: copy it.
     (hyperdrive-copy-url (cdr range-entry)))
    ((nil quote)
     ;; Known to not exist: warn user.
     (user-error "File does not exist!"))
    ((unknown quote)
     ;; Not known to exist: warn user.
     (user-error "File not known to exist!"))))

(declare-function hyperdrive-download-entry "hyperdrive")

(defun hyperdrive-history-download-file (range-entry filename)
  "Download entry in RANGE-ENTRY at point to FILENAME on disk."
  (declare (modes hyperdrive-history-mode))
  (interactive
   (pcase-let* ((range-entry (hyperdrive-history-range-entry-at-point))
                ((cl-struct hyperdrive-entry name) (cdr range-entry))
                (read-filename (when (eq t (hyperdrive-range-entry-exists-p range-entry))
                                 ;; Only prompt for filename when entry exists

                                 ;; FIXME: This function is only intended for
                                 ;; interactive use. Is it acceptable to have a nil
                                 ;; argument list and perform the user interactions
                                 ;; in the body? This change would deduplicate the
                                 ;; check for the existence of the entry.
                                 (read-file-name "Filename: "
                                                 (expand-file-name name hyperdrive-download-directory)))))
     (list range-entry read-filename)))
  (cl-ecase (hyperdrive-range-entry-exists-p range-entry)
    ((t quote)
     ;; Known to exist: download it.
     (hyperdrive-download-entry (cdr range-entry) filename))
    ((nil quote)
     ;; Known to not exist: warn user.
     (user-error "File does not exist!"))
    ((unknown quote)
     ;; Not known to exist: warn user.
     (user-error "File not known to exist!"))))

(provide 'hyperdrive-history)
;;; hyperdrive-history.el ends here
