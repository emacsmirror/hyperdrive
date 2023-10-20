;;; test-hyperdrive-markdown.el --- Test Hyperdrive.el's integration with markdown-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Joseph Turner <joseph@ushin.org>

;; Author: Joseph Turner
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <joseph@ushin.org>

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

;; This file tests Hyperdrive.el's `markdown-mode' link functionality.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'ert)
(require 'pcase)

(require 'hyperdrive)
(require 'markdown-mode)

;;;; Tests

;;;; Parse relative/absolute link into entry tests

;; Neither full "hyper://"-prefixed URLs, nor links which are only search
;; options, are handled by `hyperdrive--org-link-entry-at-point'.

(defmacro hyperdrive-test-markdown-parse-link-deftest (name current-entry link parsed-entry)
  (declare (indent defun))
  (let ((test-name (intern (format "hyperdrive-test-markdown-parse-link/%s" name))))
    `(ert-deftest ,test-name ()
       (let ((hyperdrive-current-entry ,current-entry))
         (with-temp-buffer
           ;; FIXME: Use persistent buffer for performance.
           (markdown-mode)
           (erase-buffer)
           (insert ,link)
           (goto-char (point-min))
           (should
            (hyperdrive-entry-equal-p ,parsed-entry
                                      (hyperdrive--markdown-url-entry (markdown-link-url)))))))))

(hyperdrive-test-markdown-parse-link-deftest absolute/without-search-option
  (hyperdrive-entry-create
   :hyperdrive (hyperdrive-create :public-key "deadbeef")
   :path "/foo/bar quux.md")
  "[link](</foo/bar quux.md>)"
  (hyperdrive-entry-create
   :hyperdrive (hyperdrive-create :public-key "deadbeef")
   :path "/foo/bar quux.md"))

(hyperdrive-test-markdown-parse-link-deftest parent/without-search-option
  (hyperdrive-entry-create
   :hyperdrive (hyperdrive-create :public-key "deadbeef")
   :path "/foo/bar quux.md")
  "[link](<../foo/bar quux.md>)"
  (hyperdrive-entry-create
   :hyperdrive (hyperdrive-create :public-key "deadbeef")
   :path "/foo/bar quux.md"))

(hyperdrive-test-markdown-parse-link-deftest sibling/without-search-option
  (hyperdrive-entry-create
   :hyperdrive (hyperdrive-create :public-key "deadbeef")
   :path "/foo/bar quux.md")
  "[link](<./bar quux.md>)"
  (hyperdrive-entry-create
   :hyperdrive (hyperdrive-create :public-key "deadbeef")
   :path "/foo/bar quux.md"))

;; (hyperdrive-test-markdown-parse-link-deftest sibling/with-heading-text-search-option
;;   (hyperdrive-entry-create
;;    :hyperdrive (hyperdrive-create :public-key "deadbeef")
;;    :path "/foo/bar quux.md")
;;   "[link](<./bar quux.md::Heading A>)"
;;   (hyperdrive-entry-create
;;    :hyperdrive (hyperdrive-create :public-key "deadbeef")
;;    :path "/foo/bar quux.md"
;;    :etc '((target . "Heading A"))))

;; (hyperdrive-test-markdown-parse-link-deftest sibling/with-heading-text*-search-option
;;   (hyperdrive-entry-create
;;    :hyperdrive (hyperdrive-create :public-key "deadbeef")
;;    :path "/foo/bar quux.md")
;;   "[link](<./bar quux.md::*Heading A>)"
;;   (hyperdrive-entry-create
;;    :hyperdrive (hyperdrive-create :public-key "deadbeef")
;;    :path "/foo/bar quux.md"
;;    :etc '((target . "*Heading A"))))

;; (hyperdrive-test-markdown-parse-link-deftest sibling/with-custom-id-search-option
;;   (hyperdrive-entry-create
;;    :hyperdrive (hyperdrive-create :public-key "deadbeef")
;;    :path "/foo/bar quux.md")
;;   "[link](<./bar quux.md::#baz zot>)"
;;   (hyperdrive-entry-create
;;    :hyperdrive (hyperdrive-create :public-key "deadbeef")
;;    :path "/foo/bar quux.md"
;;    :etc '((target . "#baz zot"))))
