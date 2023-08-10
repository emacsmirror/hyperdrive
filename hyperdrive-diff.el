;;; hyperdrive-diff.el --- Shared (persist-)defvars, deffaces, defcustoms  -*- lexical-binding: t; -*-

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

;; Code related to diffing hyperdrive entries.

;;; Code:

(require 'hyperdrive-lib)
(require 'hyperdrive-vars)

(require 'diff)
(require 'rx)

;;;; Commands

;;;; Internal variables

(defvar-local hyperdrive-diff-entries nil
  "Entries to be diffed in `hyperdrive-diff' buffer.
A cons cell whose car is OLD-ENTRY and whose cdr is NEW-ENTRY.")
(put 'hyperdrive-diff-entries 'permanent-local t)

;;;; Functions

(defun hyperdrive-diff-empty-diff-p (buffer)
  "Return t if `hyperdrive-diff-mode' BUFFER has no differences."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-max))
        (forward-line -1)
        (looking-at-p (rx line-start "Diff finished (no differences)."))))))

(cl-defun hyperdrive-diff-file-entries (old-entry new-entry &key then)
  "Diff OLD-ENTRY and NEW-ENTRY, then call THEN in diff buffer.
Call ELSE if either request fails.
This function is intended to diff files, not directories."
  (declare (indent defun))
  ;; Both entries should exist since we use
  ;; `hyperdrive-entry-description' to generate buffer names
  (cl-check-type old-entry hyperdrive-entry)
  (cl-check-type new-entry hyperdrive-entry)
  (require 'diff)
  (let* (old-response
         new-response
         (queue (make-plz-queue
                 :limit 2
                 :finally (lambda ()
                            (unless (or old-response new-response)
                              (hyperdrive-error "Files non-existent"))
                            (let ((old-buffer (generate-new-buffer
                                               (hyperdrive-entry-description old-entry)))
                                  (new-buffer (generate-new-buffer
                                               (hyperdrive-entry-description new-entry)))
                                  ;; TODO: Improve diff buffer name.
                                  (diff-buffer (get-buffer-create "*hyperdrive-diff*")))
                              (when old-response
                                (with-current-buffer old-buffer
                                  (insert (plz-response-body old-response))))
                              (when new-response
                                (with-current-buffer new-buffer
                                  (insert (plz-response-body new-response))))
                              (unwind-protect
                                  (condition-case err
                                      (progn
                                        (diff-no-select old-buffer new-buffer nil t diff-buffer)
                                        (with-current-buffer diff-buffer
                                          (setf hyperdrive-diff-entries (cons old-entry new-entry))
                                          (hyperdrive-diff-mode)
                                          (funcall then)))
                                    (error (kill-buffer diff-buffer)
                                           (signal (car err) (cdr err))))
                                (kill-buffer old-buffer)
                                (kill-buffer new-buffer)))))))
    (when old-entry
      (hyperdrive-api 'get (hyperdrive-entry-url old-entry)
        :queue queue :as 'response :else #'ignore
        :then (lambda (response)
                (setf old-response response))))
    (when new-entry
      (hyperdrive-api 'get (hyperdrive-entry-url new-entry)
        :queue queue :as 'response :else #'ignore
        :then (lambda (response)
                (setf new-response response))))))

;;;; Mode

(define-derived-mode hyperdrive-diff-mode diff-mode "hyperdrive-diff"
  "Major mode for `hyperdrive-diff' buffers."
  :group 'hyperdrive
  :interactive nil
  ;; Narrow the buffer to hide the diff command and "diff finished" lines.
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (delete-line)
      (when (hyperdrive-diff-empty-diff-p (current-buffer))
        (insert (format "No difference between entries:
%s
%s"
                        (hyperdrive-entry-description (car hyperdrive-diff-entries))
                        (hyperdrive-entry-description (cdr hyperdrive-diff-entries)))))
      (goto-char (point-max))
      (forward-line -1)
      (delete-region (point) (point-max)))))

;;;; Footer

(provide 'hyperdrive-diff)
;;; hyperdrive-diff.el ends here
