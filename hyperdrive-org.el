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

(defvar hyperdrive-mode)

(declare-function hyperdrive-open-url "hyperdrive")
(declare-function hyperdrive-dir--entry-at-point "hyperdrive-dir")

(defcustom hyperdrive-org-link-full-url nil
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
  (when hyperdrive-current-entry
    (pcase-let (((map type link description)
                 (pcase major-mode
                   ('org-mode (hyperdrive--org-link))
                   ('hyperdrive-dir-mode
                    (let ((entry (hyperdrive-dir--entry-at-point)))
                      `((type . "hyper://")
                        (link . ,(hyperdrive-entry-url entry))
                        (description . ,(hyperdrive-entry-description entry)))))
                   (_ `((type . "hyper://")
                        (link . ,(hyperdrive-entry-url hyperdrive-current-entry))
                        (description . ,(hyperdrive-entry-description hyperdrive-current-entry)))))))
      (org-link-store-props :type type :link link :description description)
      t)))

(defun hyperdrive--org-link (&optional raw-url-p)
  "Return Org alist for current Org buffer.
Attempts to link to the entry at point.  If RAW-URL-P, return a
raw URL, not an Org link."
  ;; NOTE: We would like to return a plist rather than an alist, but
  ;; the version of `map' included with Emacs 27 doesn't support that,
  ;; and depending on a later version won't force Emacs to actually
  ;; use it when compiling this package, so for now we avoid
  ;; destructuring plists with `pcase-let'.
  ;; NOTE: Ideally we would simply reuse Org's internal functions to
  ;; store links, like `org-store-link'.  However, its API is not
  ;; designed to be used by external libraries, and requires ugly
  ;; hacks like tricking it into thinking that the buffer has a local
  ;; filename; and even then, it doesn't seem possible to control how
  ;; it generates target fragments like we need.  So it's simpler for
  ;; us to reimplement some of the logic here.
  ;;
  ;; Also, it appears that Org links to ID properties (not CUSTOM_ID)
  ;; can't have filename parts, i.e. they can only link to the
  ;; generated ID and leave locating the entry's file to Org's cache,
  ;; which isn't suitable for our purposes.  So instead, we generate
  ;; our own link type which, in that case, includes both the filename
  ;; and the ID or CUSTOM_ID.

  ;; The URL's "fragment" (aka "target" in org-link jargon) is either
  ;; the CUSTOM_ID, ID, or headline search string, whichever is found
  ;; first, and it's up to the follow function to determine which it
  ;; is (which is very simple; see below).
  (cl-assert (eq 'org-mode major-mode))
  (when hyperdrive-mode
    (let* ((heading (org-entry-get (point) "ITEM"))
           (custom-id (org-entry-get (point) "CUSTOM_ID"))
           ;; (generated-id (org-entry-get (point) "ID"))
           (fragment (cond (custom-id (concat "#" custom-id))
                           (heading (concat "*" heading))))
           (entry-copy (hyperdrive-copy-tree hyperdrive-current-entry t))
           (_ (setf (alist-get 'target (hyperdrive-entry-etc entry-copy)) fragment))
           (raw-url (hyperdrive-entry-url entry-copy)))
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
  (hyperdrive-open (hyperdrive-url-entry (concat "hyper:" url))))

(defun hyperdrive--org-link-goto (target)
  "Go to TARGET in current Org buffer.
TARGET may be a CUSTOM_ID, an ID, or a headline."
  (cl-assert (eq 'org-mode major-mode))
  ;; We do not ensure that a target only exists once in the file, but
  ;; neither does Org always do so.
  (setf target (url-unhex-string target))
  (goto-char (or (org-find-property "CUSTOM_ID" target)
                 (org-find-property "ID" target)
                 (org-find-exact-headline-in-buffer target)
                 (hyperdrive-error "Unable to find entry in file: %S" target))))

(defun hyperdrive-org-link-complete ()
  "Create a hyperdrive org link."
  ;; TODO: Support other hyper:// links like diffs when implemented.
  (hyperdrive-entry-url (hyperdrive-read-entry :force-prompt t)))

(defun hyperdrive--org-open-at-point ()
  "Handle relative links in hyperdrive-mode org files.

Added to `org-open-at-point-functions' in order to short-circuit
the logic for handling links of \"fuzzy\" or \"file\" type.

Uses `url-default-expander' to expand the relative link against
the current location."
  (when hyperdrive-mode
    (let* ((context
            ;; TODO: Double-check that this is the correct way to get context.
            (org-element-lineage (org-element-context) '(link) t))
           (element-type (org-element-type context))
           (link-type (org-element-property :type context))
           (raw-link-type (org-element-property :raw-link context)))
      (when (and (eq element-type 'link)
                 (or
                  ;; "fuzzy" is for relative links without ./ prefix.
                  (equal "fuzzy" link-type)
                  ;; "file is for absolute links and relative links with ./ prefix.
                  (equal "file" link-type))
                 ;; Allow links to explicitly point to local files by
                 ;; prefixing with "file:" (because Org assumes that links
                 ;; without a specified protocol are "file:" links).
                 (not (string-prefix-p "file:" raw-link-type)))
        ;; FIXME: For fuzzy links, passing to hyperdrive-expand-url is a no-no.
        (hyperdrive-open-url (hyperdrive-expand-url (org-element-property :path context)))))))

(defun hyperdrive--org-insert-link-after-advice (&rest _)
  "Modify just-inserted link as appropriate for `hyperdrive-mode' buffers."
  (when (and hyperdrive-mode hyperdrive-current-entry)
    (let ((link-element (org-element-context)))
      (cl-assert (eq 'link (car link-element)))
      (delete-region (org-element-property :begin link-element)
                     (org-element-property :end link-element))
      (insert (org-link-make-string (hyperdrive--org-normalize-link link-element))))))

(cl-defun hyperdrive--org-normalize-link (link-element)
  "Return normalized copy of \"hyper://\" LINK-ELEMENT.
Respects `hyperdrive-org-link-full-url' and `org-link-file-path-type'.
FIXME: Docstring, maybe move details from `hyperdrive-org-link-full-url'."
  (cl-assert hyperdrive-current-entry)
  (let* ((url (org-element-property :raw-link link-element))
         (target-entry (hyperdrive-url-entry url))
         (search-option (alist-get 'target (hyperdrive-entry-etc target-entry)))
         (host-format '(public-key)) (with-path t) (with-protocol t)
         fragment-prefix)

    (when (and search-option
               (hyperdrive-entry-equal-p hyperdrive-current-entry target-entry))
      ;; Search option alone
      (cl-return-from hyperdrive--org-normalize-link search-option))

    (when (or hyperdrive-org-link-full-url
              (not (hyperdrive-entry-hyperdrive-equal-p
                    hyperdrive-current-entry target-entry)))
      ;; Full "hyper://" URL
      (when search-option
        (setf fragment-prefix (concat "#" (url-hexify-string "::"))))
      (cl-return-from hyperdrive--org-normalize-link
        (hyperdrive--format-entry-url
         target-entry :fragment-prefix fragment-prefix
         :with-path with-path
         :with-protocol with-protocol :host-format host-format)))

    (let ((adaptive-target-p
           ;; See the `adaptive' option in `org-link-file-path-type'.
           (string-prefix-p
            (file-name-directory
             (hyperdrive-entry-path hyperdrive-current-entry))
            (hyperdrive-entry-path target-entry))))
      (hyperdrive--ensure-dot-slash-prefix-path
       (apply #'concat
              (pcase org-link-file-path-type
                ;; TODO: Handle `org-link-file-path-type' as a function.
                ((or 'absolute
                     ;; TODO: Consider special-casing `noabbrev' - who knows?
                     ;; `noabbrev' is like `absolute' because hyperdrives have
                     ;; no home directory.
                     'noabbrev
                     (and 'adaptive (guard (not adaptive-target-p))))
                 (hyperdrive-entry-path target-entry))
                ((or 'relative (and 'adaptive (guard adaptive-target-p)))
                 (file-relative-name
                  (hyperdrive-entry-path target-entry)
                  (file-name-directory (hyperdrive-entry-path hyperdrive-current-entry)))))
              (when search-option
                (list "::" search-option)))))))

;;;###autoload
(with-eval-after-load 'org
  (org-link-set-parameters "hyper"
                           :store #'hyperdrive-org-link-store
                           :follow #'hyperdrive-org-link-follow
			   :complete #'hyperdrive-org-link-complete)
  (with-eval-after-load 'hyperdrive
    ;; Handle links with no specified type in `hyperdrive-mode'
    ;; buffers as links to files within that hyperdrive.  Only add
    ;; this function to the variable after `hyperdrive' is loaded so
    ;; that `hyperdrive-mode' will be defined.
    (cl-pushnew #'hyperdrive--org-open-at-point org-open-at-point-functions)))

;;;; Footer

(provide 'hyperdrive-org)
;;; hyperdrive-org.el ends here
