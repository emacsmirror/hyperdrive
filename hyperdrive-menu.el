;;; hyperdrive.el --- P2P filesystem  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 USHIN, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Author: Joseph Turner <joseph@ushin.org>

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

;; This file adds a transient.el menu for hyperdrive entries.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'pcase)
(require 'transient)
(require 'compat)

(require 'hyperdrive-vars)
(require 'hyperdrive-lib)

;;;; Declarations

(declare-function hyperdrive-dir--entry-at-point "hyperdrive-dir")

;;;;; hyperdrive-menu: Transient for entries

;; TODO: Use something like this later.
;; (defmacro hyperdrive-menu-lambda (&rest body)
;;   (declare (indent defun))
;;   `(lambda ()
;;      (when hyperdrive-current-entry
;;        (pcase-let (((cl-struct hyperdrive-entry hyperdrive)
;;                     hyperdrive-current-entry))
;;          ,@body))))

(transient-define-prefix hyperdrive-menu (entry)
  "Show the hyperdrive transient menu."

  [ :class transient-row
    :description
    (lambda ()
      (let ((hyperdrive (hyperdrive-entry-hyperdrive (oref transient--prefix scope))))
        (concat (propertize "Drive: " 'face 'transient-heading)
                (hyperdrive--format-hyperdrive hyperdrive :formats '(short-key seed domain nickname petname))
                ;; TODO: Consider moving the latest version number into the "Version" group.
                (format "  latest:%s" (hyperdrive-latest-version hyperdrive)))))
    ("e" "Edit hyperdrive" hyperdrive-menu-hyperdrive)
    ;; TODO: Hook into transient-show-help?
    ("?" "Info manual" hyperdrive-info-manual)]
  [[ ;; Current directory
    :if (lambda ()
          (and (oref transient--prefix scope) (eq major-mode 'hyperdrive-dir-mode)))
    :description
    (lambda ()
      (let ((entry (oref transient--prefix scope)))
        (concat (propertize "Current directory: " 'face 'transient-heading)
                (propertize (hyperdrive--format-path (hyperdrive-entry-path entry))
                            'face 'transient-value))))
    ("^" "Up to parent" hyperdrive-up)
    ("o" "Sort" hyperdrive-dir-sort)]
   [ ;; File at point
    :if (lambda ()
          (let ((entry (oref transient--prefix scope)))
            (and (oref transient--prefix scope)
                 (eq major-mode 'hyperdrive-dir-mode)
                 (when-let ((entry-at-point (hyperdrive-dir--entry-at-point)))
                   (not (hyperdrive--entry-directory-p entry-at-point))))))
    :description
    (lambda ()
      (let ((entry (oref transient--prefix scope)))
        (concat (propertize "File at point: " 'face 'transient-heading)
                (propertize (hyperdrive-entry-name (hyperdrive-dir--entry-at-point))
                            'face 'transient-value))))
    ("f d" "Download" hyperdrive-download)
    ;; FIXME: Enable this as a command.
    ;; ("f D" "Delete" hyperdrive-delete)
    ;; FIXME: Copy entry at point, not `hyperdrive-current-entry'.
    ("f w" "Copy URL" hyperdrive-copy-url)]
   [ ;; Directory at point
    :if (lambda ()
          (let ((entry (oref transient--prefix scope)))
            (and (oref transient--prefix scope)
                 (eq major-mode 'hyperdrive-dir-mode)
                 (when-let ((entry-at-point (hyperdrive-dir--entry-at-point)))
                   (hyperdrive--entry-directory-p entry-at-point)))))
    :description
    (lambda ()
      (let ((entry (oref transient--prefix scope)))
        (concat (propertize "Directory at point: " 'face 'transient-heading)
                (propertize (hyperdrive-entry-name (hyperdrive-dir--entry-at-point))
                            'face 'transient-value))))
    ;; FIXME: Enable this as a command.
    ;; ("f D" "Delete" hyperdrive-delete)
    ;; FIXME: Copy entry at point, not `hyperdrive-current-entry'.
    ("f w" "Copy URL" hyperdrive-copy-url)]
   ["Version"
    :description (lambda ()
                   (if-let ((entry (oref transient--prefix scope))
                            (hyperdrive (hyperdrive-entry-hyperdrive entry)))
                       (concat (propertize "Version: "
                                           'face 'transient-heading)
                               (propertize (format "%s"
                                                   (or (hyperdrive-entry-version entry)
                                                       "latest"))
                                           'face 'transient-value))
                     "Version"))
    ("v h" "History" hyperdrive-history)
    ("v n" "Next" hyperdrive-next-version
     :if (lambda () (oref transient--prefix scope))
     :inapt-if-not (lambda  ()
                     (let ((entry (oref transient--prefix scope)))
                       (and (hyperdrive-entry-version entry)
                            (hyperdrive-entry-next entry))))
     ;; :transient t
     :description (lambda ()
                    (concat "Next"
                            (when-let* ((entry (oref transient--prefix scope))
                                        (hyperdrive (hyperdrive-entry-hyperdrive entry))
                                        (next-entry (hyperdrive-entry-next entry))
                                        ;; Don't add ": latest" if we're already at the latest version
                                        ((not (eq entry next-entry)))
                                        (display-version (if-let ((next-version (hyperdrive-entry-version next-entry)))
                                                             (number-to-string next-version)
                                                           "latest")))
                              (concat ": " (propertize display-version 'face 'transient-value))))))
    ("v p" "Previous" hyperdrive-previous-version
     :if (lambda () (oref transient--prefix scope))
     :inapt-if-not (lambda ()
                     (hyperdrive-entry-previous (oref transient--prefix scope) :cache-only t))
     ;; :transient t
     :description (lambda ()
                    (if-let ((entry (oref transient--prefix scope))
                             (hyperdrive (hyperdrive-entry-hyperdrive entry)))
                        (concat "Previous"
                                (pcase-exhaustive (hyperdrive-entry-previous entry :cache-only t)
                                  ('unknown (concat ": " (propertize "?" 'face 'transient-value)))
                                  ('nil nil)
                                  ((cl-struct hyperdrive-entry version)
                                   (concat ": " (propertize (number-to-string version)
                                                            'face 'transient-value)))))
                      "Previous")))]]
  [["Gateway"
    ("g s" "Start" hyperdrive-start)
    ("g S" "Stop" hyperdrive-stop)
    ("g v" "Version" hyperdrive-hyper-gateway-version)]
   ["Drives"
    ;; TODO: Consider showing current drive's public key or formatted name.
    ("d n" "New" hyperdrive-new)
    ("d d" "Describe" hyperdrive-describe-hyperdrive)
    ("d P" "Purge" hyperdrive-purge)]
   ["Bookmark"
    ("b j" "Jump" hyperdrive-bookmark-jump)
    ("b l" "List" hyperdrive-bookmark-list)
    ("b s" "Set" bookmark-set)]
   ["Files"
    ("f f" "Find" hyperdrive-find-file)
    ("f v" "View" hyperdrive-view-file)
    ("f o" "Open URL" hyperdrive-open-url)]
   ["Upload"
    ("u f" "File" hyperdrive-upload-file)
    ("u F" "Files" hyperdrive-upload-files)
    ("u m" "Mirror" hyperdrive-mirror)]]
  (interactive (list hyperdrive-current-entry))
  (transient-setup 'hyperdrive-menu nil nil :scope entry))

;;;;; hyperdrive-menu-hyperdrive: Transient for hyperdrives

(transient-define-prefix hyperdrive-menu-hyperdrive (hyperdrive)
  "Show menu for editing HYPERDRIVE."
  [ :class transient-row
    :description
    (lambda ()
      (let ((hyperdrive (oref transient--prefix scope)))
        (concat (propertize "Drive: " 'face 'transient-heading)
                (hyperdrive--format-hyperdrive hyperdrive :formats '(public-key seed domain))
                (format "  latest:%s" (hyperdrive-latest-version hyperdrive)))))
    ("p" "Petname" hyperdrive-menu-set-petname
     :description (lambda ()
                    (format "Petname: %s"
                            (pcase (hyperdrive-petname
                                    (oref transient--prefix scope))
                              (`nil (propertize "none"
                                                'face 'transient-inactive-value))
                              (it (propertize it
                                              'face 'transient-value))))))
    ("n" "set nickname" hyperdrive-menu-set-nickname
     :inapt-if-not (lambda ()
                     (hyperdrive-writablep (oref transient--prefix scope)))
     :description (lambda ()
                    (format "Nickname: %s"
                            ;; TODO: Hyperdrive-metadata accessor (and maybe gv setter).
                            (pcase (alist-get 'name
                                              (hyperdrive-metadata
                                               (oref transient--prefix scope)))
                              ('nil (propertize "none"
                                                'face 'transient-inactive-value))
                              (it (propertize it
                                              'face 'transient-value))))))]
  (interactive (list (hyperdrive-complete-hyperdrive :force-prompt current-prefix-arg)))
  (transient-setup 'hyperdrive-menu-hyperdrive nil nil :scope hyperdrive))

(transient-define-suffix hyperdrive-menu-set-petname (petname)
  ;; TODO: Offer current petname as default value; note that
  ;; transient--prefix and transient-current-prefix are both nil here.
  (interactive (list (hyperdrive-read-name :prompt "New petname")))
  (let ((hyperdrive (oref transient-current-prefix scope)))
    (hyperdrive-set-petname petname hyperdrive)
    ;; TODO: Pass scope to `hyperdrive-menu-hyperdrive' so that, e.g.,
    ;; C-g correctly returns the user back to the the
    ;; `hyperdrive-menu' if that's where we came from. Same in
    ;; `hyperdrive-menu-set-nickname'.
    (hyperdrive-menu-hyperdrive hyperdrive)))

(transient-define-suffix hyperdrive-menu-set-nickname (nickname)
  ;; TODO: Offer current nickname as default value; note that
  ;; transient--prefix and transient-current-prefix are both nil here.
  (interactive (list (hyperdrive-read-name :prompt "New nickname")))
  (hyperdrive-set-nickname nickname (oref transient-current-prefix scope)
                           :then (lambda (hyperdrive)
                                   (hyperdrive-menu-hyperdrive hyperdrive))))

;;;; Footer

(provide 'hyperdrive-menu)

;;; hyperdrive-menu.el ends here
