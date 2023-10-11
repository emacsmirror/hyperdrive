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
(declare-function hyperdrive-delete "hyperdrive")
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
  [["Hyperdrive"
    :description
    (lambda ()
      (if-let* ((entry (oref transient--prefix scope))
                (hyperdrive (hyperdrive-entry-hyperdrive entry)))
          (concat (propertize "Hyperdrive: " 'face 'transient-heading)
                  (hyperdrive--format-host hyperdrive :with-label t))
        "Hyperdrive"))
    ("h" "Hyperdrive" hyperdrive-menu-hyperdrive)
    ("N" "New drive" hyperdrive-new)
    ("L" "Open Link" hyperdrive-open-url)]
   ["Version"
    :if (lambda () (and (oref transient--prefix scope)
                        ;; TODO: Remove this check and add useful history transient UI.
                        (not (eq 'hyperdrive-history-mode major-mode))))
    :description (lambda ()
                   (if-let ((entry (oref transient--prefix scope)))
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
                    (if-let ((entry (oref transient--prefix scope)))
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
                                        (next-entry (hyperdrive-entry-next entry))
                                        ;; Don't add ": latest" if we're already at the latest version
                                        ((not (eq entry next-entry)))
                                        (display-version (if-let ((next-version (hyperdrive-entry-version next-entry)))
                                                             (number-to-string next-version)
                                                           "latest")))
                              (concat ": " (propertize display-version 'face 'transient-value))))))
    ("V h" "History" hyperdrive-history)]]
  [ :if (lambda () (and (oref transient--prefix scope)
                        ;; TODO: Remove this check and add useful history transient UI.
                        (not (eq 'hyperdrive-history-mode major-mode))))
    [ ;; Current
     :description
     (lambda ()
       (let ((entry (oref transient--prefix scope)))
         (concat (propertize "Current: " 'face 'transient-heading)
                 (propertize (hyperdrive--format-path (hyperdrive-entry-path entry))
                             'face 'transient-value))))
     ("^" "Up to parent" (lambda ()
                           (interactive)
                           (hyperdrive-up (oref transient-current-prefix scope)
                                          :then (lambda ()
                                                  (call-interactively #'hyperdrive-menu))))
      :inapt-if-not (lambda ()
                      (hyperdrive-parent (oref transient--prefix scope)))
      :transient t)
     ("s" "Sort" hyperdrive-dir-sort
      :if (lambda ()
            (eq major-mode 'hyperdrive-dir-mode))
      :transient t)
     ("j" "Jump" imenu)
     ;; TODO: Combine previous and next commands on the same line?
     ;; TODO: See "predicate refreshing" <https://github.com/magit/transient/issues/157>.
     ("p" "Previous" (lambda ()
                       (interactive)
                       (hyperdrive-ewoc-previous)
                       (hyperdrive-menu (oref transient--prefix scope)))
      :if (lambda ()
            (eq major-mode 'hyperdrive-dir-mode))
      :transient t)
     ("n" "Next" (lambda ()
                   (interactive)
                   (hyperdrive-ewoc-next)
                   (hyperdrive-menu (oref transient--prefix scope)))
      :if (lambda ()
            (eq major-mode 'hyperdrive-dir-mode))
      :transient t)
     ("w" "Copy URL" hyperdrive-copy-url
      :if (lambda ()
            (not (eq major-mode 'hyperdrive-dir-mode))))
     ("D" "Delete" hyperdrive-delete
      :if (lambda ()
            (not (eq major-mode 'hyperdrive-dir-mode)))
      :inapt-if (lambda ()
                  (pcase-let (((cl-struct hyperdrive-entry hyperdrive version)
                               (oref transient--prefix scope)))
                    (or version (not (hyperdrive-writablep hyperdrive))))))
     ("d" "Download" hyperdrive-download
      :if (lambda ()
            (not (eq major-mode 'hyperdrive-dir-mode))))]
    ;; TODO: Consider adding a defcustom to hide the "Selected" and
    ;; "Current" groups when in a directory buffer.
    [ ;; Selected
     :if (lambda ()
           (and (oref transient--prefix scope)
                (eq major-mode 'hyperdrive-dir-mode)
                (hyperdrive-dir--entry-at-point)))
     :description
     (lambda ()
       (let ((current-entry (oref transient--prefix scope))
             (selected-entry (hyperdrive-dir--entry-at-point)))
         (concat (propertize "Selected: " 'face 'transient-heading)
                 (propertize
                  (or (and (hyperdrive-entry-equal-p current-entry selected-entry)
                           "./")
                      (alist-get 'display-name
                                 (hyperdrive-entry-etc selected-entry))
                      (hyperdrive-entry-name selected-entry))
                  'face 'transient-value))))
     :pad-keys t
     ("d" "Download" hyperdrive-download
      :inapt-if (lambda ()
                  (when-let ((entry-at-point (hyperdrive-dir--entry-at-point)))
                    (hyperdrive--entry-directory-p entry-at-point))))
     ("D" "Delete" hyperdrive-delete
      :inapt-if (lambda ()
                  (let ((current-entry (oref transient--prefix scope))
                        (selected-entry (hyperdrive-dir--entry-at-point)))
                    (or (not (hyperdrive-writablep
                              (hyperdrive-entry-hyperdrive current-entry)))
                        (eq selected-entry current-entry)
                        (string= "../" (alist-get 'display-name
                                                  (hyperdrive-entry-etc selected-entry)))))))
     ("w" "Copy URL" hyperdrive-dir-copy-url)
     ;; FIXME: The sequence "? ? RET" says "Unbound suffix" instead of showing the help for that command.  Might be an issue in Transient.
     ("RET" "Open" hyperdrive-dir-find-file)
     ("v" "View" hyperdrive-dir-view-file
      :inapt-if (lambda ()
                  (when-let ((entry-at-point (hyperdrive-dir--entry-at-point)))
                    (hyperdrive--entry-directory-p entry-at-point))))]]
  [["Gateway"
    :description
    (lambda ()
      (concat (propertize "Gateway: " 'face 'transient-heading)
              (propertize (if (hyperdrive-status) "on" "off")
                          'face 'transient-value)))
    ("G s" "Start" hyperdrive-start
     :transient t)
    ("G S" "Stop" hyperdrive-stop
     :transient t)
    ("G v" "Version" hyperdrive-hyper-gateway-version
     :transient t)]
   ["Bookmark"
    ("b j" "Jump" hyperdrive-bookmark-jump)
    ("b l" "List" hyperdrive-bookmark-list)
    ("b s" "Set" bookmark-set
     :if (lambda ()
           (oref transient--prefix scope)))]
   ["Upload"
    ("P f" "File" hyperdrive-upload-file)
    ("P F" "Files" hyperdrive-upload-files)
    ("P m" "Mirror" hyperdrive-mirror)]]
  (interactive (list hyperdrive-current-entry))
  (transient-setup 'hyperdrive-menu nil nil :scope entry))

;;;;; hyperdrive-menu-hyperdrive: Transient for hyperdrives

(transient-define-prefix hyperdrive-menu-hyperdrive (hyperdrive)
  "Show menu for HYPERDRIVE."
  [:description
   (lambda ()
     (let ((hyperdrive (oref transient--prefix scope)))
       (concat (propertize "Hyperdrive: " 'face 'transient-heading)
               (hyperdrive--format-hyperdrive hyperdrive :formats '(public-key seed domain))
               (format "  latest-version:%s" (hyperdrive-latest-version hyperdrive)))))
   [("f" "Find file"
     (lambda ()
       (interactive)
       (hyperdrive-open
         (hyperdrive-read-entry
          :hyperdrive (oref transient-current-prefix scope)
          :read-version current-prefix-arg))))
    ("v" "View file" (lambda ()
                       (interactive)
                       (hyperdrive-view-file
                        (hyperdrive-read-entry
                         :hyperdrive (oref transient-current-prefix scope)
                         :read-version current-prefix-arg))))]
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
