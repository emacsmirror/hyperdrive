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
(declare-function hyperdrive-set-nickname "hyperdrive")
(declare-function hyperdrive-set-petname "hyperdrive")

;;;;; hyperdrive-menu: Transient for entries

;; TODO: Use something like this later.
;; (defmacro hyperdrive-menu-lambda (&rest body)
;;   (declare (indent defun))
;;   `(lambda ()
;;      (when hyperdrive-current-entry
;;        (pcase-let (((cl-struct hyperdrive-entry hyperdrive)
;;                     hyperdrive-current-entry))
;;          ,@body))))

;; TODO: Add macro that expands `entry' into (oref transient--prefix scope)
;; or (oref transient-current-prefix scope) as appropriate.
;;;###autoload (autoload 'hyperdrive-menu "hyperdrive-menu" nil t)
(transient-define-prefix hyperdrive-menu (entry)
  "Show the hyperdrive transient menu."
  :info-manual "(Hyperdrive)"
  [ :class transient-row
    :description
    (lambda ()
      (if-let* ((entry (oref transient--prefix scope))
                (hyperdrive (hyperdrive-entry-hyperdrive entry)))
          (concat (propertize "Hyperdrive: " 'face 'transient-heading)
                  (hyperdrive--format-host hyperdrive :with-label t))
        "Hyperdrive"))
    ("h" "Hyperdrive menu" hyperdrive-menu-hyperdrive)
    ("N" "New drive" hyperdrive-new)]
  [ :if (lambda () (oref transient--prefix scope))

    ["Version"
     :class transient-row
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
     ("V p" "Previous" hyperdrive-previous-version
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
                       "Previous")))
     ("V n" "Next" hyperdrive-next-version
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
     ("V h" "History" hyperdrive-history)]
    [ ;; Current
     :description
     (lambda ()
       (let ((entry (oref transient--prefix scope)))
         (concat (propertize "Current: " 'face 'transient-heading)
                 (propertize (hyperdrive--format-path (hyperdrive-entry-path entry))
                             'face 'transient-value))))
     ("^" "Up to parent" hyperdrive-up
      :inapt-if-not (lambda ()
                      (hyperdrive-parent (oref transient--prefix scope))))
     ("o" "Sort" hyperdrive-dir-sort
      :if (lambda ()
            (eq major-mode 'hyperdrive-dir-mode))
      :transient t)
     ;; TODO: Combine previous and next commands
     ;; TODO: See "predicate refreshing" <https://github.com/magit/transient/issues/157>.
     ("p" "previous" (lambda ()
                       (interactive)
                       (hyperdrive-ewoc-previous)
                       (hyperdrive-menu (oref transient--prefix scope)))
      :if (lambda ()
            (eq major-mode 'hyperdrive-dir-mode))
      :transient t)
     ("n" "next" (lambda ()
                   (interactive)
                   (hyperdrive-ewoc-next)
                   (hyperdrive-menu (oref transient--prefix scope)))
      :if (lambda ()
            (eq major-mode 'hyperdrive-dir-mode))
      :transient t)
     ("w" "Copy URL" hyperdrive-copy-url
      :if (lambda ()
            (not (eq major-mode 'hyperdrive-dir-mode))))
     ;; FIXME: Enable this as a command.
     ;; ("D" "Delete" hyperdrive-delete)
     ("d" "Download" hyperdrive-download
      :if (lambda ()
            (not (eq major-mode 'hyperdrive-dir-mode))))]
    [ ;; Selected
     :if (lambda ()
           (and (oref transient--prefix scope)
                (eq major-mode 'hyperdrive-dir-mode)))
     :description
     (lambda ()
       (concat (propertize "Selected: " 'face 'transient-heading)
               (propertize (hyperdrive-entry-name (hyperdrive-dir--entry-at-point))
                           'face 'transient-value)))
     :pad-keys t
     ("d" "Download" hyperdrive-download
      :if (lambda ()
            (when-let ((entry-at-point (hyperdrive-dir--entry-at-point)))
              (not (hyperdrive--entry-directory-p entry-at-point)))))
     ;; FIXME: Enable this as a command.
     ;; ("D" "Delete" hyperdrive-delete)
     ;; FIXME: Copy entry at point, not `hyperdrive-current-entry'.
     ("w" "Copy URL" hyperdrive-copy-url)
     ;; FIXME: The sequence "? ? RET" says "Unbound suffix" instead of showing the help for that command.  Might be an issue in Transient.
     ("RET" "Open" hyperdrive-dir-find-file)
     ("v" "View" hyperdrive-dir-view-file
      :if (lambda ()
            (when-let ((entry-at-point (hyperdrive-dir--entry-at-point)))
              (not (hyperdrive--entry-directory-p entry-at-point)))))]]
  [["Gateway"
    ("g s" "Start" hyperdrive-start)
    ("g S" "Stop" hyperdrive-stop)
    ("g v" "Version" hyperdrive-hyper-gateway-version)]
   ["Bookmark"
    ("b j" "Jump" hyperdrive-bookmark-jump)
    ("b l" "List" hyperdrive-bookmark-list)
    ("b s" "Set" bookmark-set
     :if (lambda ()
           (oref transient--prefix scope)))]
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
  [:description
   (lambda ()
     (let ((hyperdrive (oref transient--prefix scope)))
       (concat (propertize "Hyperdrive: " 'face 'transient-heading)
               (hyperdrive--format-hyperdrive hyperdrive :formats '(public-key seed domain))
               (format "  latest-version:%s" (hyperdrive-latest-version hyperdrive)))))
   [("d" "Describe" hyperdrive-describe-hyperdrive)
    ("C-M-P" "Purge" hyperdrive-purge)]
   [("p" "Petname" hyperdrive-menu-set-petname
     :transient t
     :description (lambda ()
                    (format "Petname: %s"
                            (pcase (hyperdrive-petname
                                    (oref transient--prefix scope))
                              (`nil (propertize "none"
                                                'face 'transient-inactive-value))
                              (it (propertize it
                                              'face 'transient-value))))))
    ("n" "set nickname" hyperdrive-menu-set-nickname
     :transient t
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
                                              'face 'transient-value))))))]]
  (interactive (list (hyperdrive-complete-hyperdrive :force-prompt current-prefix-arg)))
  (transient-setup 'hyperdrive-menu-hyperdrive nil nil :scope hyperdrive))

(transient-define-suffix hyperdrive-menu-set-petname (petname)
  (interactive
   (list (hyperdrive-read-name
          :prompt "New petname"
          :initial-input (hyperdrive-petname (oref transient-current-prefix scope)))))
  (let ((hyperdrive (oref transient-current-prefix scope)))
    (hyperdrive-set-petname petname hyperdrive)))

(transient-define-suffix hyperdrive-menu-set-nickname (nickname)
  (interactive
   (list (hyperdrive-read-name
          :prompt "New nickname"
          :initial-input (alist-get 'name (hyperdrive-metadata (oref transient-current-prefix scope))))))
  (hyperdrive-set-nickname nickname (oref transient-current-prefix scope)
                           :then (lambda (hyperdrive)
                                   (hyperdrive-menu-hyperdrive hyperdrive))))

;;;; Footer

(provide 'hyperdrive-menu)

;;; hyperdrive-menu.el ends here
