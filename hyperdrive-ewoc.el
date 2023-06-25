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
  "Move forward N entries."
  (declare (modes hyperdrive-ewoc-mode))
  (interactive "p")
  (cond ((= 1 (line-number-at-pos))
         ;; Point on header: move into first entry.
         (forward-line 1))
        (t
         ;; Point is elsewhere: move to next entry (`ewoc-next' won't
         ;; move past the last entry).
         (hyperdrive-ewoc-move n))))

(cl-defun hyperdrive-ewoc-previous (&optional (n 1))
  "Move backward N entries."
  (declare (modes hyperdrive-ewoc-mode))
  (interactive "p")
  (cond ((= 2 (line-number-at-pos))
         ;; Point on first entry: move into header.
         (forward-line -1))
        ((> (line-number-at-pos)
            (line-number-at-pos (ewoc-location (ewoc-nth hyperdrive-ewoc -1))))
         ;; Point is past last entry: move to last entry.
         (goto-char (ewoc-location (ewoc-nth hyperdrive-ewoc -1))))
        (t
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
