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
(require 'map)

(require 'hyperdrive-lib)
(require 'hyperdrive-ewoc)

;;;; Variables

(defvar hyperdrive-current-entry)
(defvar hyperdrive-timestamp-format)
(defvar hyperdrive-default-host-format)
(defvar hyperdrive-history-display-buffer-action)

;;;; Functions

(defun hyperdrive-history-pp (thing)
  "Pretty-print THING.
To be used as the pretty-printer for `ewoc-create'."
  (pcase-exhaustive thing
    ((pred hyperdrive-entry-p)
     (insert (hyperdrive-history--format-entry thing)))))

(defun hyperdrive-history--format-entry (entry)
  "Return ENTRY formatted as a string."
  (pcase-let* (((cl-struct hyperdrive-entry size modified) entry)
               (`(,range-start . ,(map (:existsp existsp) (:range-end range-end)))
                (hyperdrive-entry-version-range entry))
               (range (format "%d-%d" range-start range-end))
               (size (when size
                       (file-size-human-readable size)))
               (timestamp (if modified
                              (format-time-string hyperdrive-timestamp-format modified)
                            (format hyperdrive-timestamp-format-string " "))))
    ;; FIXME: Use dynamic width of range column equal to 2N+1, where N
    ;; is the width of the hyperdrive's latest version
    (if existsp
        (format "%10s  %6s  %s"
                (propertize range
                            ;; TODO: Another font for range?
                            'face 'hyperdrive-size)
                (propertize (or size "")
                            'face 'hyperdrive-size)
                (propertize timestamp
                            'face 'hyperdrive-timestamp))
      (format "%10s  does not exist"
              (propertize range
                          ;; TODO: Another font for range?
                          'face 'hyperdrive-size)))))

(defun hyperdrive-history--entry-at-point ()
  "Return entry at version at point.
With point on header or below last entry, return nil."
  (unless (or
           ;; Point on header.
           (= 1 (line-number-at-pos))
           ;; Point is below the last entry.
           (> (line-number-at-pos)
              (line-number-at-pos (ewoc-location (ewoc-nth hyperdrive-ewoc -1)))))
    ;; Point on a file version: check that it exists.
    (pcase-let* ((entry (ewoc-data (ewoc-locate hyperdrive-ewoc)))
                 (`(_range-start . ,(map (:existsp existsp))) (hyperdrive-entry-version-range entry)))
      (when existsp entry))))

;;;; Mode

(defvar-keymap hyperdrive-history-mode-map
  :parent  hyperdrive-ewoc-mode-map
  :doc "Local keymap for `hyperdrive-history-mode' buffers.")

(define-derived-mode hyperdrive-history-mode hyperdrive-ewoc-mode
  `("Hyperdrive-history"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive history buffers."
  (setf hyperdrive-ewoc (ewoc-create #'hyperdrive-history-pp)
        hyperdrive-ewoc--entry-at-point #'hyperdrive-history--entry-at-point))

;;;; Commands

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
               (entries
                (mapcar (pcase-lambda (`(,range-start . _plist))
                          ;; Some entries may not exist at
                          ;; `range-start', as in the version before
                          ;; it was created. See manual:
                          ;; [[info:hyperdrive-manual.info#Versioning]]
                          (hyperdrive-entry-create
                           :hyperdrive hyperdrive
                           :path path
                           :version range-start))
                        (hyperdrive-entry-version-ranges entry)))
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
      (mapc (lambda (entry)
              (ewoc-enter-last hyperdrive-ewoc entry))
            entries)
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
      (mapc (lambda (entry)
              (when (hyperdrive-entry-p entry)
                ;; TODO: Handle failures?
                (hyperdrive-fill entry :queue queue :then #'ignore)))
            entries)
      (set-buffer-modified-p nil)
      (goto-char (point-min)))))

(provide 'hyperdrive-history)
;;; hyperdrive-history.el ends here
