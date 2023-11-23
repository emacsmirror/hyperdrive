;;; hyperdrive-org.el --- Org-related functionality  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USHIN, Inc.

;; Author: Adam Porter <adam@alphapapa.net>

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

;; This file contains Org mode-related functionality.

;;; Code:

;;;; Requirements

(require 'org)
(require 'org-element)

(require 'hyperdrive-lib)

(defvar h/mode)

(declare-function h/open-url "hyperdrive")
(declare-function h/dir--entry-at-point "hyperdrive-dir")

(defcustom h/org-link-full-url nil
  "Always insert full \"hyper://\" URLs when linking to hyperdrive files.
Otherwise, when inserting a link to the same hyperdrive Org file,

- insert a relative path link when before the first heading, or
- insert a heading text or CUSTOM_ID link when after the first heading

Otherwise, when inserting a link to a different file in the same
hyperdrive, insert a relative or absolute link according to
`org-link-file-path-type'."
  :type 'boolean
  :group 'hyperdrive)

;; TODO: Determine whether it's really necessary to autoload these two functions.

;;;###autoload
(defun hyperdrive-org-link-store ()
  "Store an Org link to the entry at point in current Org buffer.
To be called by `org-store-link'.  Calls `org-link-store-props',
which see."
  (when h/current-entry
    (pcase-let (((map type link description)
                 (pcase major-mode
                   ('org-mode (h/org--link))
                   ('h/dir-mode
                    (let ((entry (h/dir--entry-at-point)))
                      `((type . "hyper://")
                        (link . ,(he/url entry))
                        (description . ,(h//format-entry entry)))))
                   (_ `((type . "hyper://")
                        (link . ,(he/url h/current-entry))
                        (description . ,(h//format-entry h/current-entry)))))))
      (org-link-store-props :type type :link link :description description)
      t)))

(defun h/org--link (&optional raw-url-p)
  "Return Org alist for current Org buffer.
Attempts to link to the entry at point.  If RAW-URL-P, return a
raw URL, not an Org link."
  ;; TODO: Since we depend on Emacs 28 now, we can rely on `map'
  ;; being able to destructure a plist inside `pcase-let', so we
  ;; should switch to using a plist instead of an alist.
  ;; NOTE: Ideally we would simply reuse Org's internal functions to
  ;; store links, like `org-store-link'.  However, its API is not
  ;; designed to be used by external libraries, and requires ugly
  ;; hacks like tricking it into thinking that the buffer has a local
  ;; filename; and even then, it doesn't seem possible to control how
  ;; it generates target fragments like we need.  So it's simpler for
  ;; us to reimplement some of the logic here.
  ;;
  ;; The URL's "fragment" (aka "target" in org-link jargon) is the
  ;; CUSTOM_ID if it exists or headline search string if it exists.
  (cl-assert (eq 'org-mode major-mode))
  (when h/mode
    (let* ((heading (org-entry-get (point) "ITEM"))
           (custom-id (org-entry-get (point) "CUSTOM_ID"))
           (fragment (cond (custom-id (concat "#" custom-id))
                           (heading (concat "*" heading))))
           (entry-copy (h/copy-tree h/current-entry t))
           (_ (setf (alist-get 'target (he/etc entry-copy)) fragment))
           (raw-url (he/url entry-copy)))
      (if raw-url-p
          raw-url
        ;; NOTE: Due to annoying issues with older versions of Emacs
        ;; that have older versions of map.el that don't support
        ;; destructuring plists with pcase-let, we use an alist here.
        `((type . "hyper") (link . ,raw-url) (description . ,heading))))))

;;;###autoload
(defun hyperdrive-org-link-follow (url &optional _prefix)
  ;; TODO: Do we need to do anything if prefix is used?
  "Follow hyperdrive URL."
  ;; Add "hyper:" prefix because Org strips the prefix for links that
  ;; have been configured with `org-link-set-parameters'.
  (h/open (h/url-entry (concat "hyper:" url))))

(defun h/org--link-goto (target)
  "Go to TARGET in current Org buffer.
TARGET may be a CUSTOM_ID or a headline."
  (cl-assert (eq 'org-mode major-mode))
  (org-link-search target))

(defun h/org-link-complete ()
  "Create a hyperdrive org link."
  ;; TODO: Support other hyper:// links like diffs when implemented.
  (he/url (h/read-entry :read-version t)))

(defun h/org--open-at-point ()
  "Handle relative links in hyperdrive-mode org files.

Added to `org-open-at-point-functions' in order to short-circuit
the logic for handling links of \"file\" type."
  (when-let ((h/mode)
             (link (h/org--link-entry-at-point)))
    (h/open link)))

(defun h/org--link-entry-at-point ()
  "Return a hyperdrive entry for the Org link at point."
  ;; This function is not in the code path for full URLs or links that
  ;; are only search options.
  (let* ((context (org-element-lineage (org-element-context) '(link) t))
         (element-type (org-element-type context))
         (link-type (org-element-property :type context))
         (raw-link-type (org-element-property :raw-link context)))
    (when (and (eq element-type 'link)
               (equal "file" link-type)
               ;; Don't treat link as a relative/absolute path in the
               ;; hyperdrive if "file:" protocol prefix is explicit.
               (not (string-prefix-p "file:" raw-link-type)))
      (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) h/current-entry)
                   (entry (he/create
                           :hyperdrive hyperdrive
                           :path (expand-file-name (org-element-property :path context)
                                                   (file-name-directory path))
                           :etc `((target . ,(org-element-property :search-option context))))))
        entry))))

(defun h/org--insert-link-after-advice (&rest _)
  "Modify just-inserted link as appropriate for `hyperdrive-mode' buffers."
  (when (and h/mode h/current-entry)
    (let* ((link-element (org-element-context))
           (_ (cl-assert (eq 'link (car link-element))))
           (url (org-element-property :raw-link link-element))
           (desc (h/org--link-description link-element))
           (target-entry (h/url-entry url)))
      (when (and (not h/org-link-full-url)
                 (he/hyperdrive-equal-p
                  h/current-entry target-entry))
        (delete-region (org-element-property :begin link-element)
                       (org-element-property :end link-element))
        (insert (org-link-make-string
                 (h/org--shorthand-link target-entry)
                 desc))))))

(cl-defun h/org--shorthand-link (entry)
  "Return a non-\"hyper://\"-prefixed link to ENTRY.
Respects `hyperdrive-org-link-full-url' and `org-link-file-path-type'."
  ;; FIXME: Docstring, maybe move details from `h/org-link-full-url'.
  (cl-assert h/current-entry)
  (let ((search-option (alist-get 'target (he/etc entry))))
    (when (and search-option
               (he/equal-p h/current-entry entry))
      (cl-return-from h/org--shorthand-link search-option))

    ;; Search option alone: Remove leading "::"
    (when search-option
      (cl-callf2 concat "::" search-option))

    (let ((adaptive-target-p
           ;; See the `adaptive' option in `org-link-file-path-type'.
           (string-prefix-p
            (file-name-directory
             (he/path h/current-entry))
            (he/path entry))))
      (h//ensure-dot-slash-prefix-path
       (concat
        (pcase org-link-file-path-type
          ;; TODO: Handle `org-link-file-path-type' as a function.
          ((or 'absolute
               ;; TODO: Consider special-casing `noabbrev' - who knows?
               ;; `noabbrev' is like `absolute' because hyperdrives have
               ;; no home directory.
               'noabbrev
               (and 'adaptive (guard (not adaptive-target-p))))
           (he/path entry))
          ((or 'relative (and 'adaptive (guard adaptive-target-p)))
           (file-relative-name
            (he/path entry)
            (file-name-directory (he/path h/current-entry)))))
        search-option)))))

(defun h/org--link-description (link)
  "Return description of Org LINK or nil if it has none."
  ;; TODO: Is there a built-in solution?
  (when-let* ((desc-begin (org-element-property :contents-begin link))
              (desc-end (org-element-property :contents-end link)))
    (buffer-substring desc-begin desc-end)))

;;;###autoload
(with-eval-after-load 'org
  (org-link-set-parameters "hyper"
                           :store #'h/org-link-store
                           :follow #'h/org-link-follow
			   :complete #'h/org-link-complete)
  (with-eval-after-load 'hyperdrive
    ;; Handle links with no specified type in `h/mode'
    ;; buffers as links to files within that hyperdrive.  Only add
    ;; this function to the variable after `hyperdrive' is loaded so
    ;; that `h/mode' will be defined.
    (cl-pushnew #'h/org--open-at-point org-open-at-point-functions)))

;;;; Footer

(provide 'hyperdrive-org)

;;;###autoload(register-definition-prefixes "hyperdrive-org" '("hyperdrive-"))
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive-org.el ends here
