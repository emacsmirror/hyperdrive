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

;;;; Commands

;;;; Functions

(cl-defun hyperdrive-diff-file-entries (old-entry new-entry &key then else)
  "Diff OLD-ENTRY and NEW-ENTRY, then call THEN on diff buffer.
Call ELSE if either request fails.  ELSE may potentially be called
twice, once per failed request.
This function is intended to diff files, not directories."
  (declare (indent defun))
  (require 'diff)
  (let* (old-buffer
         new-buffer
         (diff-buffer (get-buffer-create "*hyperdrive-diff*"))
         (queue (make-plz-queue :limit 2
                                :finally (lambda ()
                                           (funcall then (let ((diff-entire-buffers nil))
                                                           (diff-no-select old-buffer new-buffer nil nil diff-buffer)))))))
    (if old-entry
        (hyperdrive-api 'get (hyperdrive-entry-url old-entry)
          :queue queue :as 'buffer :else else
          :then (lambda (buffer)
                  (setf old-buffer buffer)))
      (setf old-buffer (generate-new-buffer "old-entry-nonexistent")))
    (if new-entry
        (hyperdrive-api 'get (hyperdrive-entry-url new-entry)
          :queue queue :as 'buffer :else else
          :then (lambda (buffer)
                  (setf new-buffer buffer)))
      (setf new-buffer (generate-new-buffer "new-entry-nonexistent")))))

;;;; Footer

(provide 'hyperdrive-diff)
;;; hyperdrive-diff.el ends here
