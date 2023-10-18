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

;;;; hyperdrive-menu: Transient for entries

;; TODO: Use something like this later.
;; (defmacro hyperdrive-menu-lambda (&rest body)
;;   (declare (indent defun))
;;   `(lambda ()
;;      (when hyperdrive-current-entry
;;        (pcase-let (((cl-struct hyperdrive-entry hyperdrive)
;;                     hyperdrive-current-entry))
;;          ,@body))))

;;;###autoload (autoload 'hyperdrive-menu "hyperdrive-menu" nil t)
(transient-define-prefix hyperdrive-menu (entry)
  "Show the hyperdrive transient menu."
  :info-manual "(hyperdrive)"
  :refresh-suffixes t
  [["Hyperdrive"
    :description
    (lambda ()
      (if-let* ((entry (hyperdrive-menu--entry))
                (hyperdrive (hyperdrive-entry-hyperdrive entry)))
          (concat (propertize "Hyperdrive: " 'face 'transient-heading)
                  (hyperdrive--format-host hyperdrive :with-label t))
        "Hyperdrive"))
    ("h" "Hyperdrive" hyperdrive-menu-hyperdrive)
    ("N" "New drive" hyperdrive-new)
    ("L" "Open Link" hyperdrive-open-url)]
   ["Version"
    :if (lambda ()
          (and (hyperdrive-menu--entry)
               ;; TODO: Remove this check and add useful history transient UI.
               (not (eq 'hyperdrive-history-mode major-mode))))
    :description (lambda ()
                   (if-let ((entry (hyperdrive-menu--entry)))
                       (concat (propertize "Version: "
                                           'face 'transient-heading)
                               (propertize (format "%s"
                                                   (or (hyperdrive-entry-version entry)
                                                       "latest"))
                                           'face 'transient-value))
                     "Version"))
    ("V p" "Previous" hyperdrive-open-previous-version
     :inapt-if-not (lambda ()
                     (hyperdrive-entry-previous (hyperdrive-menu--entry) :cache-only t))
     ;; :transient t
     :description (lambda ()
                    (if-let ((entry (hyperdrive-menu--entry)))
                        (concat "Previous"
                                (pcase-exhaustive (hyperdrive-entry-previous entry :cache-only t)
                                  ('unknown (concat ": " (propertize "?" 'face 'transient-value)))
                                  ('nil nil)
                                  ((cl-struct hyperdrive-entry version)
                                   (concat ": " (propertize (number-to-string version)
                                                            'face 'transient-value)))))
                      "Previous")))
    ("V n" "Next" hyperdrive-open-next-version
     :inapt-if-not (lambda  ()
                     (let ((entry (hyperdrive-menu--entry)))
                       (and (hyperdrive-entry-version entry)
                            (hyperdrive-entry-p (hyperdrive-entry-next entry)))))
     :description (lambda ()
                    (concat "Next"
                            (when-let* ((entry (hyperdrive-menu--entry))
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
                 (hyperdrive--entry-directory-p (hyperdrive-menu--entry))))]]
  [:if (lambda ()
         (and (hyperdrive-menu--entry)
              ;; TODO: Remove this check and add useful history transient UI.
              (not (eq 'hyperdrive-history-mode major-mode))))
   [;; Current
    :description
    (lambda ()
      (let ((entry (hyperdrive-menu--entry)))
        (concat (propertize "Current: " 'face 'transient-heading)
                (propertize (hyperdrive--format-path (hyperdrive-entry-path entry))
                            'face 'transient-value))))
    ("g" "Refresh" revert-buffer)
    ("^" "Up to parent"
     (lambda ()
       (interactive)
       (hyperdrive-up (hyperdrive-menu--entry)))
     :inapt-if-not (lambda ()
                     (hyperdrive-parent (hyperdrive-menu--entry))))
    ("s" "Sort" hyperdrive-dir-sort
     :if-mode hyperdrive-dir-mode
     :transient t)
    ("j" "Jump" imenu)
    ;; TODO: Combine previous and next commands on the same line?
    ("p" "Previous" hyperdrive-ewoc-previous
     :if-mode hyperdrive-dir-mode
     :transient t)
    ("n" "Next" hyperdrive-ewoc-next
     :if-mode hyperdrive-dir-mode
     :transient t)
    ("w" "Copy URL" hyperdrive-copy-url
     :if-not-mode hyperdrive-dir-mode)
    ("D" "Delete" hyperdrive-delete
     :if-not-mode hyperdrive-dir-mode
     :inapt-if (lambda ()
                 (pcase-let (((cl-struct hyperdrive-entry hyperdrive version)
                              (hyperdrive-menu--entry)))
                   (or version (not (hyperdrive-writablep hyperdrive))))))
    ("d" "Download" hyperdrive-download
     :if-not-mode hyperdrive-dir-mode)]
   ;; TODO: Consider adding a defcustom to hide the "Selected" and
   ;; "Current" groups when in a directory buffer.
   [;; Selected
    :if (lambda ()
          (and (hyperdrive-menu--entry)
               (eq major-mode 'hyperdrive-dir-mode)
               (hyperdrive-dir--entry-at-point)))
    :description
    (lambda ()
      (let ((current-entry (hyperdrive-menu--entry))
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
                 (let ((current-entry (hyperdrive-menu--entry))
                       (selected-entry (hyperdrive-dir--entry-at-point)))
                   (or (not (hyperdrive-writablep
                             (hyperdrive-entry-hyperdrive current-entry)))
                       (eq selected-entry current-entry)
                       (string= "../" (alist-get 'display-name
                                                 (hyperdrive-entry-etc selected-entry)))))))
    ("w" "Copy URL" hyperdrive-dir-copy-url)
    ;; FIXME: The sequence "? RET" says "Unbound suffix" instead of showing the help for that command.  Might be an issue in Transient.
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
     :if hyperdrive-menu--entry)]]
  (interactive (list hyperdrive-current-entry))
  (transient-setup 'hyperdrive-menu nil nil :scope entry))

;;;; hyperdrive-menu-hyperdrive: Transient for hyperdrives

(transient-define-prefix hyperdrive-menu-hyperdrive (hyperdrive)
  "Show menu for HYPERDRIVE."
  :refresh-suffixes t
  ["Hyperdrive"
   :pad-keys t
   ("d" "Describe" (lambda ()
                     (interactive)
                     (hyperdrive-describe-hyperdrive (hyperdrive-menu--entry))))
   (:info (lambda () (concat "Public key: " (hyperdrive--format-host (hyperdrive-menu--entry) :format 'public-key))))
   (:info (lambda () (concat "Seed: " (hyperdrive--format-host (hyperdrive-menu--entry) :format 'seed)))
    :if (lambda () (hyperdrive-seed (hyperdrive-menu--entry))))
   ("p" "Petname" hyperdrive-menu-set-petname
    :transient t
    :description (lambda ()
                   (format "Petname: %s"
                           (if-let ((petname (hyperdrive-petname
                                              (hyperdrive-menu--entry))))
                               (propertize petname
                                           'face 'hyperdrive-petname)
                             ""))))
   ("n" "set nickname" hyperdrive-menu-set-nickname
    :transient t
    :inapt-if-not (lambda ()
                    (hyperdrive-writablep (hyperdrive-menu--entry)))
    :description (lambda ()
                   (format "Nickname: %s"
                           ;; TODO: Hyperdrive-metadata accessor (and maybe gv setter).
                           (if-let ((nickname (alist-get 'name
                                                         (hyperdrive-metadata
                                                          (hyperdrive-menu--entry)))))
                               (propertize nickname
                                           'face 'hyperdrive-nickname)
                             ""))))
   (:info (lambda () (concat "Domain: " (hyperdrive--format-host (hyperdrive-menu--entry) :format 'domain)))
    :if (lambda () (hyperdrive-domains (hyperdrive-menu--entry))))
   (:info (lambda () (format "Latest version: %s" (hyperdrive-latest-version (hyperdrive-menu--entry)))))]
  [["Open"
    ("f" "Find file"
     (lambda ()
       (interactive)
       (hyperdrive-open
         (hyperdrive-read-entry
          :hyperdrive (hyperdrive-menu--entry)
          :read-version current-prefix-arg))))
    ("v" "View file" (lambda ()
                       (interactive)
                       (hyperdrive-view-file
                        (hyperdrive-read-entry
                         :hyperdrive (hyperdrive-menu--entry)
                         :read-version current-prefix-arg))))]
   ["Upload"
    ("u f" "File" hyperdrive-menu-upload-file
     :inapt-if-not (lambda ()
                     (hyperdrive-writablep (hyperdrive-menu--entry))))
    ("u F" "Files" hyperdrive-menu-upload-files
     :inapt-if-not (lambda ()
                     (hyperdrive-writablep (hyperdrive-menu--entry))))
    ;; TODO: When `hyperdrive-mirror' is rewritten with transient.el, set the hyperdrive by default to the
    ("u m" "Mirror" hyperdrive-mirror
     :inapt-if-not (lambda ()
                     (hyperdrive-writablep (hyperdrive-menu--entry))))]]
  (interactive (list (hyperdrive-complete-hyperdrive :force-prompt current-prefix-arg)))
  (transient-setup 'hyperdrive-menu-hyperdrive nil nil :scope hyperdrive))

(transient-define-suffix hyperdrive-menu-upload-file (filename entry)
  (interactive
   (let* ((filename (read-file-name "Upload file: "))
          (entry (hyperdrive-read-entry :hyperdrive (hyperdrive-menu--entry)
                                        :default-path (file-name-nondirectory filename)
                                        :latest-version t)))
     (list filename entry)))
  (hyperdrive-upload-file filename entry))

(transient-define-suffix hyperdrive-menu-upload-files (files hyperdrive &key target-directory)
  (interactive
   (let ((drive (hyperdrive-menu--entry)))
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
          :initial-input (hyperdrive-petname (hyperdrive-menu--entry)))
         (hyperdrive-menu--entry)))
  (hyperdrive-set-petname petname hyperdrive))

(transient-define-suffix hyperdrive-menu-set-nickname (nickname hyperdrive)
  (interactive
   (list (hyperdrive-read-name
          :prompt "New nickname"
          :initial-input (alist-get 'name (hyperdrive-metadata (hyperdrive-menu--entry))))
         (hyperdrive-menu--entry)))
  (hyperdrive-set-nickname nickname hyperdrive
                           :then (lambda (drive)
                                   (hyperdrive-menu-hyperdrive drive))))

;;;; Menu Utilities

(defun hyperdrive-menu--entry ()
  "Return the current entry as understood by `hyperdrive-menu'."
  (oref (or transient--prefix transient-current-prefix) scope))

;;;; Footer

(provide 'hyperdrive-menu)

;;; hyperdrive-menu.el ends here
