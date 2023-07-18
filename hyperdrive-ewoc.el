;;; hyperdrive-ewoc.el --- Common EWOC behavior for Hyperdrive  -*- lexical-binding: t; -*-

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
(require 'ewoc)

(require 'hyperdrive-lib)

;;;; Variables

(defvar-local hyperdrive-ewoc nil
  "EWOC for current hyperdrive buffer.")

;;;; Mode

(defvar-keymap hyperdrive-ewoc-mode-map
  :parent  special-mode-map
  :doc "Local keymap for `hyperdrive-ewoc-mode' buffers."
  "n"   #'hyperdrive-ewoc-next
  "p"   #'hyperdrive-ewoc-previous)

(define-derived-mode hyperdrive-ewoc-mode special-mode
  `("Hyperdrive-ewoc"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for Hyperdrive ewoc buffers."
  :interactive nil
  (hl-line-mode))

;;;; Commands

(cl-defun hyperdrive-ewoc-next (&optional (n 1))
  "Move forward N entries.
When on header line, moves point to first entry, skipping over
column headers when `hyperdrive-column-headers' is non-nil."
  (declare (modes hyperdrive-ewoc-mode))
  (interactive "p")
  ;; TODO: Try using the intangible text property on headers to
  ;; automatically skip over them without conditional code. Setting
  ;; `cursor-intangible' on the column header causes `hl-line-mode' to
  ;; highlight the wrong line when crossing over the headers.
  (pcase (line-number-at-pos)
    (1
     ;; Point on header: move into first entry.
     (forward-line (if hyperdrive-column-headers 2 1)))
    ((and 2 (guard hyperdrive-column-headers))
     ;; Point on column headers: move into first entry.
     (forward-line 1))
    (_
     ;; Point is elsewhere: move to next entry (`ewoc-next' won't
     ;; move past the last entry).
     (hyperdrive-ewoc-move n))))

(cl-defun hyperdrive-ewoc-previous (&optional (n 1))
  "Move backward N entries.
When on first entry, moves point to header line, skipping over
column headers when `hyperdrive-column-headers' is non-nil."
  (declare (modes hyperdrive-ewoc-mode))
  (interactive "p")
  (pcase (line-number-at-pos)
    (2
     ;; Point on first entry or column headers: move into header.
     (forward-line -1))
    ((and 3 (guard hyperdrive-column-headers))
     ;; Point on column headers: move into header.
     (forward-line -2))
    ((pred (< (line-number-at-pos (ewoc-location (ewoc-nth hyperdrive-ewoc -1)))))
     ;; Point is past last entry: move to last entry.
     (goto-char (ewoc-location (ewoc-nth hyperdrive-ewoc -1))))
    (_
     ;; Point is elsewhere: move to previous entry.
     (hyperdrive-ewoc-move (- n)))))

(cl-defun hyperdrive-ewoc-move (&optional (n 1))
  "Move forward N entries."
  (let ((next-fn (pcase n
                   ((pred (< 0)) #'ewoc-next)
                   ((pred (> 0)) #'ewoc-prev)))
        (node (ewoc-locate hyperdrive-ewoc))
        (i 0)
        (n (abs n))
        target-node)
    (while (and (< i n)
                (setf node (funcall next-fn hyperdrive-ewoc node)))
      (setf target-node node)
      (cl-incf i))
    (when target-node
      (goto-char (ewoc-location target-node)))))

(provide 'hyperdrive-ewoc)
;;; hyperdrive-ewoc.el ends here
