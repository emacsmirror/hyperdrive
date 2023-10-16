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
  :info-manual "(hyperdrive-manual)"
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
                            (hyperdrive-entry-p (hyperdrive-entry-next entry)))))
     :description (lambda ()
                    (concat "Next"
                            (when-let* ((entry (oref transient--prefix scope))
                                        (next-entry (hyperdrive-entry-next entry))
                                        ;; Don't add ": latest" if we're already at the latest
                                        ;; version or if the next version is `unknown'.
                                        ((and (hyperdrive-entry-version entry)
                                              (hyperdrive-entry-p (hyperdrive-entry-next entry))))
                                        (display-version (if-let ((next-version (hyperdrive-entry-version next-entry)))
                                                             (number-to-string next-version)
                                                           "latest")))
                              (concat ": " (propertize display-version 'face 'transient-value)))))
     )
    ("V a" "At..." hyperdrive-open-at-version)
    ("V h" "History" hyperdrive-history
     :inapt-if (lambda ()
                 (hyperdrive--entry-directory-p (oref transient--prefix scope))))]]
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
     ("g" "Refresh" revert-buffer)
     ("^" "Up to parent"
      (lambda ()
        (interactive)
        (hyperdrive-up (oref transient-current-prefix scope)))
      :inapt-if-not (lambda ()
                      (hyperdrive-parent (oref transient--prefix scope))))
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
           (oref transient--prefix scope)))]]
  (interactive (list hyperdrive-current-entry))
  (transient-setup 'hyperdrive-menu nil nil :scope entry))

;;;;; hyperdrive-menu-hyperdrive: Transient for hyperdrives

(transient-define-prefix hyperdrive-menu-hyperdrive (hyperdrive)
  "Show menu for HYPERDRIVE."
  ["Hyperdrive"
   :pad-keys t
   ("d" "Describe" (lambda ()
                     (interactive)
                     (hyperdrive-describe-hyperdrive (oref transient-current-prefix scope))))
   ;; FIXME: Is there a better way to intersperse lines of description and commands?
   ("" "Public key" ignore
    :description (lambda ()
                   (concat "Public key: " (hyperdrive--format-host (oref transient--prefix scope) :format 'public-key))))
   ("" "Seed" ignore
    :description (lambda ()
                   (concat "Seed: " (hyperdrive--format-host (oref transient--prefix scope) :format 'seed)))
    :if (lambda ()
          (hyperdrive-seed (oref transient--prefix scope))))
   ("p" "Petname" hyperdrive-menu-set-petname
    :transient t
    :description (lambda ()
                   (format "Petname: %s"
                           (if-let ((petname (hyperdrive-petname
                                              (oref transient--prefix scope))))
                               (propertize petname
                                           'face 'hyperdrive-petname)
                             ""))))
   ("n" "set nickname" hyperdrive-menu-set-nickname
    :transient t
    :inapt-if-not (lambda ()
                    (hyperdrive-writablep (oref transient--prefix scope)))
    :description (lambda ()
                   (format "Nickname: %s"
                           ;; TODO: Hyperdrive-metadata accessor (and maybe gv setter).
                           (if-let ((nickname (alist-get 'name
                                                         (hyperdrive-metadata
                                                          (oref transient--prefix scope)))))
                               (propertize nickname
                                           'face 'hyperdrive-nickname)
                             ""))))
   ("" "Domain" ignore
    :description (lambda ()
                   (concat "Domain: " (hyperdrive--format-host (oref transient--prefix scope) :format 'domain)))
    :if (lambda ()
          (hyperdrive-domains (oref transient--prefix scope))))
   ("" "Latest version" ignore
    :description (lambda ()
                   (format "Latest version: %s" (hyperdrive-latest-version (oref transient--prefix scope)))))]
  [["Open"
    ("f" "Find file"
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
   ["Upload"
    ("u f" "File" hyperdrive-menu-upload-file
     :inapt-if-not (lambda ()
                     (hyperdrive-writablep (oref transient--prefix scope))))
    ("u F" "Files" hyperdrive-menu-upload-files
     :inapt-if-not (lambda ()
                     (hyperdrive-writablep (oref transient--prefix scope))))
    ;; TODO: When `hyperdrive-mirror' is rewritten with transient.el, set the hyperdrive by default to the
    ("u m" "Mirror" hyperdrive-mirror
     :inapt-if-not (lambda ()
                     (hyperdrive-writablep (oref transient--prefix scope))))]]
  (interactive (list (hyperdrive-complete-hyperdrive :force-prompt current-prefix-arg)))
  (transient-setup 'hyperdrive-menu-hyperdrive nil nil :scope hyperdrive))

(transient-define-suffix hyperdrive-menu-upload-file (filename entry)
  (interactive
   (let* ((filename (read-file-name "Upload file: "))
          (entry (hyperdrive-read-entry :hyperdrive (oref transient-current-prefix scope)
                                        :default-path (file-name-nondirectory filename)
                                        :latest-version t)))
     (list filename entry)))
  (hyperdrive-upload-file filename entry))

(transient-define-suffix hyperdrive-menu-upload-files (files hyperdrive &key target-directory)
  (interactive
   (let ((drive (oref transient-current-prefix scope)))
     (list
      (hyperdrive-read-files)
      drive
      :target-directory (hyperdrive-read-path
                         :hyperdrive drive
                         :prompt "Target directory in «%s»"
                         :default "/"))))
  (hyperdrive-upload-files files hyperdrive
                           :target-directory target-directory))

(transient-define-suffix hyperdrive-menu-set-petname (petname hyperdrive)
  (interactive
   (list (hyperdrive-read-name
          :prompt "New petname"
          :initial-input (hyperdrive-petname (oref transient-current-prefix scope)))
         (oref transient-current-prefix scope)))
  (hyperdrive-set-petname petname hyperdrive))

(transient-define-suffix hyperdrive-menu-set-nickname (nickname hyperdrive)
  (interactive
   (list (hyperdrive-read-name
          :prompt "New nickname"
          :initial-input (alist-get 'name (hyperdrive-metadata (oref transient-current-prefix scope))))
         (oref transient-current-prefix scope)))
  (hyperdrive-set-nickname nickname hyperdrive
                           :then (lambda (drive)
                                   (hyperdrive-menu-hyperdrive drive))))

;;;; Footer

(provide 'hyperdrive-menu)

;;; hyperdrive-menu.el ends here
