;;; hyperdrive-download-monitor.el ---    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <~ushin/ushin@lists.sr.ht>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Monitor a file download's progress in a buffer.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'pcase)

;;;; Variables

(defvar-local h/download-monitor-etc nil)

;;;; Functions

(cl-defun h/download-monitor
    (&key buffer-name path total-size setup-fn canceled-fn
          (update-interval 1) (completed-fn
                               (lambda ()
                                 (let ((buffer (current-buffer)))
                                   (quit-window)
                                   (kill-buffer buffer)))))
  "Return buffer that monitors the download to PATH."
  (let* ((buffer (generate-new-buffer buffer-name)))
    (with-current-buffer buffer
      (setf (map-elt h/download-monitor-etc :path) path
            (map-elt h/download-monitor-etc :total-size) total-size
            (map-elt h/download-monitor-etc :completed-fn) completed-fn
            (map-elt h/download-monitor-etc :canceled-fn) canceled-fn)
      (h/download-monitor-update buffer)
      (setf (map-elt h/download-monitor-etc 'timer)
            (run-at-time update-interval update-interval
                         #'h/download-monitor-update buffer))
      (when setup-fn
        (funcall setup-fn)))
    buffer))

(defun h/download-monitor-update (buffer)
  (with-current-buffer buffer
    (pcase-let* (((map :path :total-size :completed-fn)
                  h/download-monitor-etc))
      (if (file-exists-p path)
          (let* ((attributes (file-attributes path))
                 (current-size (file-attribute-size attributes)))
            (if (= current-size total-size)
                ;; Download complete.
                (funcall (map-elt h/download-monitor-etc :completed-fn))
              ;; Download in progress: update buffer.
              (erase-buffer)
              (insert "Downloading:\n\n"
                      "File: " path "\n"
                      "Downloaded: " (file-size-human-readable current-size nil " ")
                      " / " (file-size-human-readable total-size) "\n")))
        ;; Download completed or canceled.
        ;; FIXME: We just assume here that it completed and wasn't canceled.
        (when completed-fn
          (funcall completed-fn))))))

(provide 'hyperdrive-download-monitor)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive-download-monitor.el ends here
