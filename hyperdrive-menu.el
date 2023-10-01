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

(transient-define-suffix hyperdrive-menu-up ()
  ;; :transient 'transient--do-call
  (interactive)
  (hyperdrive-menu (hyperdrive-parent (oref transient-current-prefix scope))))

(transient-define-prefix hyperdrive-menu (entry)
  "Show the hyperdrive transient menu."

  [ :class transient-row
    :description
    (lambda ()
      (let ((hyperdrive (hyperdrive-entry-hyperdrive (oref transient--prefix scope))))
        (concat (propertize "Drive: " 'face 'transient-heading)
                (hyperdrive--format-hyperdrive hyperdrive :formats '(short-key seed domain nickname petname))
                (format "  latest:%s" (hyperdrive-latest-version hyperdrive)))))
    ("s p" "Petname" hyperdrive-set-petname
     :if-non-nil hyperdrive-current-entry
     :description (lambda ()
                    (format "Petname: %s"
                            (pcase (hyperdrive-petname
                                    (hyperdrive-entry-hyperdrive (oref transient--prefix scope)))
                              (`nil (propertize "none"
                                                'face 'transient-inactive-value))
                              (it (propertize it
                                              'face 'transient-value))))))
    ("s n" "Nickname" hyperdrive-set-nickname
     :if (lambda () (and (oref transient--prefix scope)
                         (hyperdrive-writablep (hyperdrive-entry-hyperdrive (oref transient--prefix scope)))))
     :description (lambda ()
                    (format "Nickname: %s"
                            ;; TODO: Hyperdrive-metadata accessor (and maybe gv setter).
                            (pcase (alist-get 'name
                                              (hyperdrive-metadata
                                               (hyperdrive-entry-hyperdrive (oref transient--prefix scope))))
                              (`nil (propertize "none"
                                                'face 'transient-inactive-value))
                              (it (propertize it
                                              'face 'transient-value))))))
    ("?" "Info manual" hyperdrive-info-manual)]
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
    ("f o" "Open URL" hyperdrive-open-url)
    ("o" "Sort" hyperdrive-dir-sort
     :if-mode hyperdrive-dir-mode)]
   ["Upload"
    ("u f" "File" hyperdrive-upload-file)
    ("u F" "Files" hyperdrive-upload-files)
    ("u m" "Mirror" hyperdrive-mirror)]]
  [ ;; :class transient-row
   :description
   (lambda ()
     (let ((entry (oref transient--prefix scope)))
       (concat (propertize (if (hyperdrive--entry-directory-p entry)
                               "Directory" "File")
                           'face 'transient-heading)
               ": "
               (propertize (hyperdrive--format-path (hyperdrive-entry-path entry))
                           'face 'transient-value))))
   [ ;; "File"
    :if (lambda ()
          (let ((entry (oref transient--prefix scope)))
            (or (and entry
                     (not (hyperdrive--entry-directory-p entry)))
                (and (eq major-mode 'hyperdrive-dir-mode)
                     (hyperdrive-dir--entry-at-point)))))
    ("f d" "Download" hyperdrive-download)
    ;; FIXME: Enable this as a command.
    ;; ("f D" "Delete" hyperdrive-delete)

    ("f ^" "Up to parent" hyperdrive-menu-up)
    ("f w" "Copy URL" hyperdrive-copy-url)
    ;; ("f g"
    ;; TODO: Consider whether we want to have a menu entry for revert-buffer.
    ;;  ;; TODO: Learn how to use `transient-setup-children' to
    ;;  ;; set up this group at runtime and include the default
    ;;  ;; `revert-buffer' binding.
    ;;  revert-buffer :description "Revert")
    ("f s" "Save"
     ;; TODO: Learn how to use `transient-setup-children' to
     ;; set up this group at runtime and include the default
     ;; `save-buffer' binding.
     save-buffer)
    ("f W"
     ;; TODO: Learn how to use `transient-setup-children' to
     ;; set up this group at runtime and include the default
     ;; `write-buffer' binding.
     hyperdrive-write-buffer :description "Write")]
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
     :if-not (lambda () (oref transient--prefix scope))
     :inapt-if-not (lambda  ()
                     (hyperdrive-entry-version (hyperdrive-entry-next (oref transient--prefix scope))))
     ;; :transient t
     :description (lambda ()
                    (if-let ((entry (oref transient--prefix scope))
                             (hyperdrive (hyperdrive-entry-hyperdrive entry)))
                        (concat "Next" (when-let ((version (hyperdrive-entry-version (hyperdrive-entry-next entry))))
                                         (concat ": " (propertize (number-to-string version)
                                                                  'face 'transient-value))))
                      "Next")))
    ("v p" "Previous" hyperdrive-previous-version
     :if-not (lambda () (oref transient--prefix scope))
     :inapt-if-not (lambda  ()
                     (pcase (hyperdrive-entry-previous (oref transient--prefix scope) :cache-only t)
                       ('unknown nil)
                       (it (hyperdrive-entry-version it))))
     ;; :transient t
     :description (lambda ()
                    (if-let ((entry (oref transient--prefix scope))
                             (hyperdrive (hyperdrive-entry-hyperdrive entry)))
                        (concat "Previous" (when-let ((version (hyperdrive-entry-version (hyperdrive-entry-previous entry))))
                                             (concat ": " (propertize (number-to-string version)
                                                                      'face 'transient-value))))
                      "Previous")))]]
  (interactive (list (hyperdrive--context-entry)))
  (transient-setup 'hyperdrive-menu nil nil :scope entry))

;;;;; hyperdrive-menu-hyperdrive: Transient for hyperdrives

(transient-define-prefix hyperdrive-menu-hyperdrive (hyperdrive)
  "Show menu for editing HYPERDRIVE."
  [ :description
    (lambda ()
      (let ((hyperdrive (oref transient--prefix scope)))
        (concat (propertize "Drive: " 'face 'transient-heading)
                (hyperdrive--format-hyperdrive hyperdrive :formats '(public-key seed domain))
                (format "  latest:%s" (hyperdrive-latest-version hyperdrive)))))
    ("n" "set nickname" hyperdrive-menu-hyperdrive-set-nickname
     :if (lambda ()
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

(transient-define-suffix hyperdrive-menu-hyperdrive-set-nickname (nickname)
  ;; TODO: Offer current nickname as default value; note that
  ;; transient--prefix and transient-current-prefix are both nil here.
  (interactive (list (hyperdrive-read-name :prompt "New nickname")))
  (hyperdrive-set-nickname nickname (oref transient-current-prefix scope)
                           :then (lambda (hyperdrive)
                                   (hyperdrive-menu-hyperdrive hyperdrive))))

;;;; Footer

(provide 'hyperdrive-menu)

;;; hyperdrive-menu.el ends here
