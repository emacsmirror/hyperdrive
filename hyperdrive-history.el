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

;;;; Variables

(defvar hyperdrive-current-entry)
(defvar hyperdrive-timestamp-format)
(defvar hyperdrive-default-host-format)
(defvar hyperdrive-history-display-buffer-action)
(defvar hyperdrive-download-directory)

;;;; Functions

(defun hyperdrive-history-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  ;; FIXME: Perform type-checking? If not, is this function necessary?
  (insert (hyperdrive-history--format-range-entry thing)))

(defun hyperdrive-history--format-range-entry (range-entry)
  "Return RANGE-ENTRY formatted as a string.
RANGE-ENTRY is a cons cell whose car is a plist with two keys:

:range-end (whose value is a number)
:existsp (whose value may be t, nil, or unknown)

and who cdr is a hyperdrive entry. The entry's version is used as
the range start."
  (pcase-let* ((`(,(map (:range-end range-end) (:existsp existsp)) . ,entry) range-entry)
               ((cl-struct hyperdrive-entry size modified (version range-start)) entry)
               (range (if (eq range-start range-end)
                          (format "%d" range-start)
                        (format "%d-%d" range-start range-end)))
               (size (when size
                       (file-size-human-readable size)))
               (timestamp (if modified
                              (format-time-string hyperdrive-timestamp-format modified)
                            (format hyperdrive-timestamp-format-string " "))))
    ;; FIXME: Use dynamic width of range column equal to 2N+1, where N
    ;; is the width of the hyperdrive's latest version
    ;; TODO: Define these ?/X/Y faces.
    (pcase existsp
      ('unknown ; Not known whether it exists or not
       (format "%s  %10s          unknown"
               (propertize "?"
                           'face '(:foreground "black" :background "yellow"))
               (propertize range
                           ;; TODO: Another font for range?
                           'face 'hyperdrive-size)))
      ('nil ; Known to not exist
       (format "%s  %10s          nonexistent"
               (propertize "X"
                           'face '(:foreground "black" :background "red"))
               (propertize range
                           ;; TODO: Another font for range?
                           'face 'hyperdrive-size)))
      ('t ; Known to exist
       (format "%s  %10s  %6s  %s"
               (propertize "Y"
                           'face '(:foreground "black" :background "green"))
               (propertize range
                           ;; TODO: Another font for range?
                           'face 'hyperdrive-size)
               (propertize (or size "")
                           'face 'hyperdrive-size)
               (propertize timestamp
                           'face 'hyperdrive-timestamp))))))

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
             (setf (map-elt range :range-end) nil)
             (setf (hyperdrive-entry-version entry) nil)
             (cons range entry)))
          ((> current-line last-line)
           ;; Point is below the last entry: return nil.
           nil)
          (t
           ;; Point on a file entry: return its entry.
           current-range-entry))))

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
  (setf hyperdrive-ewoc (ewoc-create #'hyperdrive-history-pp)))

;;;; Commands

;;;###autoload
(defun hyperdrive-history (entry)
  "Display version history for current hyperdrive ENTRY.

Prefix argument forces `hyperdrive-read-entry' to prompt for an
entry."
  ;; TODO: Deduplicate `hyperdrive-history' and
  ;; `hyperdrive-handler-directory'. Move some of the logic into
  ;; hyperdrive-ewoc.el
  ;; TODO: Traverse history backward from known existent ranges.
  (interactive (list (if current-prefix-arg
                         (hyperdrive-read-entry :force-prompt t :allow-version-p nil)
                       hyperdrive-current-entry)))
  ;; TODO: Highlight range for ENTRY
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
               (range-entries
                (mapcar (pcase-lambda (`(,range-start . ,range))
                          ;; Some entries may not exist at
                          ;; `range-start', as in the version before
                          ;; it was created. See manual:
                          ;; [[info:hyperdrive-manual.info#Versioning]]
                          (cons range
                                (hyperdrive-entry-create
                                 :hyperdrive hyperdrive
                                 :path path
                                 :version range-start)))
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
      (mapc (pcase-lambda (`(,_range . ,entry))
              (when (eq t (hyperdrive-entry-exists-p entry))
                ;; TODO: Handle failures?
                (hyperdrive-fill entry :queue queue :then #'ignore)))
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
  (let ((entry (cdr range-entry)))
    (when (eq t (hyperdrive-entry-exists-p entry)) (hyperdrive-open entry))))

(declare-function hyperdrive-copy-url "hyperdrive")

(defun hyperdrive-history-copy-url (range-entry)
  "Copy URL of entry in RANGE-ENTRY into the kill ring."
  (declare (modes hyperdrive-history-mode))
  (interactive (list (hyperdrive-history-range-entry-at-point)))
  (let ((entry (cdr range-entry)))
    (when (eq t (hyperdrive-entry-exists-p entry)) (hyperdrive-copy-url entry))))

(declare-function hyperdrive-download-entry "hyperdrive")

(defun hyperdrive-history-download-file (range-entry filename)
  "Download entry in RANGE-ENTRY at point to FILENAME on disk."
  (declare (modes hyperdrive-history-mode))
  (interactive
   (pcase-let* ((range-entry (hyperdrive-history-range-entry-at-point))
                ((cl-struct hyperdrive-entry name) (cdr range-entry))
                (read-filename (read-file-name "Filename: " (expand-file-name name hyperdrive-download-directory))))
     (list range-entry read-filename)))
  (let ((entry (cdr range-entry)))
    (when (eq t (hyperdrive-entry-exists-p entry)) (hyperdrive-download-entry entry filename))))

(provide 'hyperdrive-history)
;;; hyperdrive-history.el ends here
