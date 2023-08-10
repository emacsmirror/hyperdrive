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
(declare-function hyperdrive-purge "hyperdrive")

;;;###autoload
(defun hyperdrive-describe-hyperdrive (hyperdrive)
  "Display various information about HYPERDRIVE.

Universal prefix argument \\[universal-argument] forces
`hyperdrive-complete-hyperdrive' to prompt for a hyperdrive."
  ;; TODO: Display latest known version of hyperdrive? Should we
  ;; store/persist that info in the hyperdrive struct?
  (interactive (list (hyperdrive-complete-hyperdrive :force-prompt current-prefix-arg)))
  ;; TODO: Do we want to asynchronously fill the hyperdrive's latest version?
  (hyperdrive-fill-latest-version hyperdrive)
  (with-current-buffer (get-buffer-create
                        (format "*Hyperdrive: %s*"
                                (hyperdrive--format-host hyperdrive :format '(short-key)
                                                         :with-label t)))
    (hyperdrive-describe-mode)
    (setq-local hyperdrive-describe-current-hyperdrive hyperdrive)
    (pcase-let (((cl-struct hyperdrive metadata domains writablep) hyperdrive)
                (inhibit-read-only t))
      (erase-buffer)
      (insert
       (propertize "Hyperdrive: \n" 'face 'bold)
       (format "Public key: %s\n" (hyperdrive--format-host hyperdrive :format '(public-key)))
       (format "Seed: %s\n" (or (hyperdrive--format-host hyperdrive :format '(seed))
                                "[none]"))
       (format "Petname: %s\n" (or (hyperdrive--format-host hyperdrive :format '(petname))
                                   "[none]"))
       (format "Nickname: %s\n" (or (hyperdrive--format-host hyperdrive :format '(nickname))
                                    "[none]"))
       (format "Domains: %s\n"
               (if domains
                   (string-join (mapcar (lambda (domain)
                                          (propertize domain 'face 'hyperdrive-domain))
                                        domains)
                                ", ")
                 "[none]"))
       (format "Latest version: %s\n" (hyperdrive-latest-version hyperdrive))
       (format "Writable: %s\n" (if writablep "yes" "no"))
       ;; TODO: Consider removing metadata table since we already display nickname above
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
                 "[none]"))
       "\n")
      (hyperdrive-insert-button "=== PURGE DATA ==="
                                'action (lambda (_button)
                                          (hyperdrive-purge hyperdrive))
                                'face 'hyperdrive-button-dangerous))
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
