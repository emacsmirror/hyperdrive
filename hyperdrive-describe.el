;;; hyperdrive-describe.el --- Display information about a hyperdrive  -*- lexical-binding: t; -*-

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

;;;; Variables

(defvar-local hyperdrive-describe-current-hyperdrive nil
  "Hyperdrive for current `hyperdrive-describe-mode' buffer.")
(put 'hyperdrive-describe-current-hyperdrive 'permanent-local t)

;;;; Commands

(declare-function org-table-align "org-table")

;;;###autoload
(defun hyperdrive-describe-hyperdrive (hyperdrive)
  "Display various information about HYPERDRIVE.

Universal prefix argument \\[universal-argument] forces
`hyperdrive-complete-hyperdrive' to prompt for a hyperdrive."
  (interactive (list (hyperdrive-complete-hyperdrive :force-prompt current-prefix-arg)))
  ;; TODO: Do we want to asynchronously fill the hyperdrive's latest version?
  (hyperdrive-fill-latest-version hyperdrive)
  (with-current-buffer (get-buffer-create
                        (format "*Hyperdrive: %s*" (hyperdrive--format hyperdrive "%k")))
    (with-silent-modifications
      (hyperdrive-describe-mode)
      (setq-local hyperdrive-describe-current-hyperdrive hyperdrive)
      (pcase-let (((cl-struct hyperdrive metadata writablep) hyperdrive))
        (erase-buffer)
        (insert
         (propertize "Hyperdrive: \n" 'face 'bold)
         (hyperdrive--format hyperdrive "Public key %K:\n" hyperdrive-raw-formats)
         (hyperdrive--format hyperdrive "Seed: %S\n" hyperdrive-raw-formats)
         (hyperdrive--format hyperdrive "Petname: %P\n" hyperdrive-raw-formats)
         (hyperdrive--format hyperdrive "Nickname: %N\n" hyperdrive-raw-formats)
         (hyperdrive--format hyperdrive "Domains: %D\n" hyperdrive-raw-formats)
         (format "Latest version: %s\n" (hyperdrive-latest-version hyperdrive))
         (format "Writable: %s\n" (if writablep "yes" "no"))
         (format "Metadata: %s\n"
                 (if metadata
                     (with-temp-buffer
                       (require 'org)
                       (org-mode)
                       (insert "\n|-\n| Key | Value |\n|-\n")
                       (cl-loop for (key . value) in metadata
                                do (insert (format "| %s | %s |\n" key value)))
                       (insert "|-\n")
                       (forward-line -1)
                       (org-table-align)
                       (buffer-string))
                   "[none]")))))
    (setq buffer-read-only t)
    (pop-to-buffer (current-buffer))))

;;;; Mode

(defun hyperdrive-describe-revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `hyperdrive-describe-mode' buffer.
Gets latest metadata from hyperdrive."
  (hyperdrive-fill-metadata hyperdrive-describe-current-hyperdrive)
  (hyperdrive-describe-hyperdrive hyperdrive-describe-current-hyperdrive))

(define-derived-mode hyperdrive-describe-mode special-mode
  `("Hyperdrive-describe"
    ;; TODO: Add more to lighter, e.g. URL.
    )
  "Major mode for buffers for describing hyperdrives."
  :group 'hyperdrive
  :interactive nil
  (setq-local revert-buffer-function #'hyperdrive-describe-revert-buffer))

;;;; Footer

(provide 'hyperdrive-describe)
;;; hyperdrive-describe.el ends here
