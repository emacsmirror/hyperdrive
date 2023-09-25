;;; test-hyperdrive-org-link.el --- Tests for Hyperdrive.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joseph Turner <joseph@ushin.org>

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

;; This file tests Hyperdrive.el's Org link functionality.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'ert)
(require 'pcase)

(require 'with-simulated-input)

(require 'hyperdrive)
(require 'hyperdrive-org)

(cl-defun hyperdrive-test-org-link-roundtrip
    (contents &key store-from insert-into)
  (declare (indent defun))
  (let ((org-id-link-to-org-use-id nil)
        (default-directory "/")
        ;; (org-link-file-path-type
        ;;  (lambda (path)
        ;;    (replace-regexp-in-string (rx bos (optional "file:")
        ;;                                  "/hyper:/")
        ;;                              "hyper://" path)))
        ;; (org-link-file-path-type
        ;;  (lambda (path)
        ;;    (string-trim-left (file-relative-name path)
        ;;                      (rx "file:"))))
        (store-from-entry (hyperdrive-entry-create
                           :hyperdrive (hyperdrive-create :public-key (car store-from))
                           :path (cdr store-from)))
        (insert-into-entry (hyperdrive-entry-create
                            :hyperdrive (hyperdrive-create :public-key (car insert-into))
                            :path (cdr insert-into)))
        org-stored-links)
    (with-temp-buffer
      (insert contents)
      (org-mode)
      (hyperdrive-mode)
      (setq-local hyperdrive-current-entry store-from-entry)
      (goto-char (point-min))
      (re-search-forward (rx "<|>"))
      (org-store-link nil 'interactive))
    (with-temp-buffer
      (org-mode)
      (hyperdrive-mode)
      (setq-local hyperdrive-current-entry insert-into-entry)
      (with-simulated-input "RET"
        (org-insert-link))
      (buffer-substring-no-properties (point-min) (point-max)))))

;; (while (re-search-forward (rx (0+ blank) (group (or "+" "-" "*")) (1+ blank)) nil t)
;;   (replace-match (make-string (length (match-string 1)) ?\; )))

;; TODO: Rename this file to test-hyperdrive-org.el

;;;; Tests

;;;;; Scenarios

(defvar hyperdrive-test-org-store-link-scenarios
  '((org-mode-before-heading
     :public-key "deadbeef"
     :path "/foo/bar quux.org"
     :content "★
* Heading A"
     :url "hyper://deadbeef/foo/bar%20quux.org"
     :desc nil)
    (org-mode-on-heading-with-custom-id
     :public-key "deadbeef"
     :path "/foo/bar quux.org"
     :content
     "* Heading A
:PROPERTIES:
:CUSTOM_ID: baz zot
:END:
★"
     :url "hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot"
     :desc "Heading A")
    (org-mode-on-heading-no-custom-id
     :public-key "deadbeef"
     :path "/foo/bar quux.org"
     :content "* Heading A
★"
     :url "hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A"
     :desc "Heading A"))
  "Alist keyed by scenario symbols.
Each value is a plist with the following keys:

- \\=`:public-key'
- \\=`:path'
- \\=`:content'
- \\=`:url'
- \\=`:desc'")

;;;;; Storing links

(cl-defun hyperdrive-test-org-store-link (contents &key public-key path)
  "Return stored link to entry with PUBLIC-KEY, PATH, and CONTENTS.
Point is indicated by ★."
  (declare (indent defun))
  (let ((org-id-link-to-org-use-id nil)
        ;; (default-directory "/")
        (entry (hyperdrive-entry-create
                :hyperdrive (hyperdrive-create :public-key public-key)
                :path path))
        org-stored-links)
    (with-temp-buffer
      (insert contents)
      (org-mode)
      (hyperdrive-mode)
      (setq-local hyperdrive-current-entry entry)
      (goto-char (point-min))
      (search-forward "★")
      (org-store-link nil 'interactive))
    org-stored-links))

(defmacro hyperdrive-test-org-store-link-deftest (scenario)
  "Test scenario in `hyperdrive-test-org-store-link-scenarios'."
  (let ((test-name (intern
                    (format "hyperdrive-test-org-store-link/%s" scenario))))
    `(ert-deftest ,test-name ()
       (pcase-let* (((map :public-key :path :content
                          (:url expected-url) (:desc expected-desc))
                     ;; TODO: Is there a better syntax that explicit `quote'?
                     (alist-get (quote ,scenario)
                                hyperdrive-test-org-store-link-scenarios))
                    (`((,got-url ,got-desc))
                     (hyperdrive-test-org-store-link content
                       :public-key public-key :path path)))
         (should (string= expected-url got-url))
         (should (string= expected-desc got-desc))))))

;; TODO: Loop through `hyperdrive-test-org-store-link-scenarios'?
(hyperdrive-test-org-store-link-deftest org-mode-before-heading)
(hyperdrive-test-org-store-link-deftest org-mode-on-heading-with-custom-id)
(hyperdrive-test-org-store-link-deftest org-mode-on-heading-no-custom-id)

;;;;; Inserting links

(cl-defun hyperdrive-test-org-entry-create (&key public-key path)
  (hyperdrive-entry-create
   :hyperdrive (hyperdrive-create :public-key public-key)
   :path path))

(cl-defun hyperdrive-test-org-insert-link-string (scenario &key public-key path)
  "Return link for SCENARIO inserted into entry with PUBLIC-KEY and PATH."
  (declare (indent defun))
  (pcase-let* (((map :url :desc) (alist-get scenario hyperdrive-test-org-store-link-scenarios))
               (org-stored-links `((,url ,desc))))
    (with-temp-buffer
      (org-mode)
      (hyperdrive-mode)
      (setq-local hyperdrive-current-entry (hyperdrive-test-org-entry-create
                                            :public-key public-key :path path))
      (org-insert-link nil url desc)
      (buffer-string))))

(cl-defmacro hyperdrive-test-org-insert-link-deftest (name &key public-key path results)
  "Test inserted link in entry with PUBLIC-KEY and PATH.
Scenario is the first part of NAME, and RESULTS contain let-bound
variables and the expected link."
  (declare (indent defun))
  (let ((scenario (intern (string-trim-right (symbol-name name)
                                             (rx "/" (1+ anything) eos))))
        body-forms)
    (pcase-dolist ((map (:let vars) (:result result)) results)
      (let* ((olfpt (cadadr (assoc 'org-link-file-path-type vars)))
             (test-name (intern (format "hyperdrive-test-org-insert-link/%s/%s"
                                        name olfpt))))
        (push `(ert-deftest ,test-name ()
                 (let (,@vars)
                   (should (string= ,result
                                    (hyperdrive-test-org-insert-link-string ',scenario
                                      :public-key ,public-key :path ,path)))))
              body-forms)))
    `(progn ,@body-forms)))

(hyperdrive-test-org-insert-link-deftest org-mode-before-heading/same-drive-same-path
  :public-key "deadbeef"
  :path "/foo/bar quux.org"
  :results (( :let ((org-link-file-path-type 'relative))
              :result "[[./bar quux.org]]")
            ( :let ((org-link-file-path-type 'absolute))
              :result "[[/foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'noabbrev))
              :result "[[/foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'adaptive))
              :result "[[./bar quux.org]]")))

(hyperdrive-test-org-insert-link-deftest org-mode-on-heading-with-custom-id/same-drive-same-path
  :public-key "deadbeef"
  :path "/foo/bar quux.org"
  :results (( :let ((org-link-file-path-type 'relative))
              :result "[[#baz zot][Heading A]]")
            ( :let ((org-link-file-path-type 'absolute))
              :result "[[#baz zot][Heading A]]")
            ( :let ((org-link-file-path-type 'noabbrev))
              :result "[[#baz zot][Heading A]]")
            ( :let ((org-link-file-path-type 'adaptive))
              :result "[[#baz zot][Heading A]]")))

(hyperdrive-test-org-insert-link-deftest org-mode-on-heading-no-custom-id/same-drive-same-path
  :public-key "deadbeef"
  :path "/foo/bar quux.org"
  :results (( :let ((org-link-file-path-type 'relative))
              :result "[[*Heading A][Heading A]]")
            ( :let ((org-link-file-path-type 'absolute))
              :result "[[*Heading A][Heading A]]")
            ( :let ((org-link-file-path-type 'noabbrev))
              :result "[[*Heading A][Heading A]]")
            ( :let ((org-link-file-path-type 'adaptive))
              :result "[[*Heading A][Heading A]]")))

(hyperdrive-test-org-insert-link-deftest org-mode-before-heading/same-drive-different-path
  :public-key "deadbeef"
  :path "/thud.org"
  :results (( :let ((org-link-file-path-type 'relative))
              :result "[[./foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'absolute))
              :result "[[/foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'noabbrev))
              :result "[[/foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'adaptive))
              :result "[[./foo/bar quux.org]]")))

(hyperdrive-test-org-insert-link-deftest org-mode-on-heading-with-custom-id/same-drive-different-path
  :public-key "deadbeef"
  :path "/thud.org"
  :results (( :let ((org-link-file-path-type 'relative))
              :result "[[./foo/bar quux.org::#baz zot][Heading A]]")
            ( :let ((org-link-file-path-type 'absolute))
              :result "[[/foo/bar quux.org::#baz zot][Heading A]]")
            ( :let ((org-link-file-path-type 'noabbrev))
              :result "[[/foo/bar quux.org::#baz zot][Heading A]]")
            ( :let ((org-link-file-path-type 'adaptive))
              :result "[[./foo/bar quux.org::#baz zot][Heading A]]")))

(hyperdrive-test-org-insert-link-deftest org-mode-on-heading-no-custom-id/same-drive-different-path
  :public-key "deadbeef"
  :path "/thud.org"
  :results (( :let ((org-link-file-path-type 'relative))
              :result "[[./foo/bar quux.org::*Heading A][Heading A]]")
            ( :let ((org-link-file-path-type 'absolute))
              :result "[[/foo/bar quux.org::*Heading A][Heading A]]")
            ( :let ((org-link-file-path-type 'noabbrev))
              :result "[[/foo/bar quux.org::*Heading A][Heading A]]")
            ( :let ((org-link-file-path-type 'adaptive))
              :result "[[./foo/bar quux.org::*Heading A][Heading A]]")))

;; + Hyperdrive Org links :: Links to hyperdrive files/directories that are valid within Org documents.

;;   - With protocol prefix

;;     This link type or may not be surrounded by brackets. It may or may
;;     not contain a search option. Path and search option must be
;;     URL-encoded and separated by a decoded ~#~.

;;     * No search option :: e.g. ~hyper://deadbeef/foo/bar%20quux.org~, which decodes to ~hyper://deadbeef/foo/bar quux.org~

(cl-defmacro hyperdrive-test-org-link-deftest (name &key store-body store-from insert-into results)
  "FIXME: Docstring."
  (declare (indent defun)
	   ;; (debug (&define symbolp &rest [&or [":store-body" stringp]
	   ;; 				      [":store-from" sexp]
	   ;; 				      [":insert-into" sexp]
	   ;; 				      [":results" sexp]]))
	   )
  (let (body-forms)
    (pcase-dolist ((map (:let vars) (:result result)) results)
      (let* ((olfpt (cadadr (assoc 'org-link-file-path-type vars)))
             (holfu (cadr (assoc 'hyperdrive-org-link-full-url vars)))
             (test-name (intern (format "hyperdrive-test-org-link/%s/%s-%s"
                                        name olfpt holfu))))
        (push `(ert-deftest ,test-name ()
                 "Docstring."
                 (let (,@vars)
                   (should (equal ,result
                                  (hyperdrive-test-org-link-roundtrip ,store-body
                                    :store-from ,store-from :insert-into ,insert-into)))))
              body-forms)))
    `(progn ,@(nreverse body-forms))))

;; TODO: We'll need at least one test for inserting a link into an Org
;; file that is /not/ in a hyperdrive.

(hyperdrive-test-org-link-deftest same-drive-same-path-before-heading
  :store-body "<|>
* Heading A
:PROPERTIES:
:CUSTOM_ID: example ID
:END:
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("deadbeef" . "/foo/bar quux.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[./bar quux.org]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[/foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[/foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[./bar quux.org]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")))

(hyperdrive-test-org-link-deftest same-drive-same-path-on-heading-with-custom-id
  :store-body "
* Heading A
:PROPERTIES:
:CUSTOM_ID: baz zot
:END:
<|>
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("deadbeef" . "/foo/bar quux.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[#baz zot]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[#baz zot]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[#baz zot]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[#baz zot]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")))

(hyperdrive-test-org-link-deftest same-drive-same-path-on-heading-no-custom-id
  :store-body "
* Heading A
<|>
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("deadbeef" . "/foo/bar quux.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[*Heading A]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[*Heading A]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[*Heading A]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[*Heading A]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")))

(hyperdrive-test-org-link-deftest same-drive-different-path-before-heading
  :store-body "<|>
* Heading A
:PROPERTIES:
:CUSTOM_ID: example ID
:END:
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("deadbeef" . "/thud.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[./foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[/foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[/foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[./foo/bar quux.org]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")))

(hyperdrive-test-org-link-deftest same-drive-different-path-on-heading-with-custom-id
  :store-body "
* Heading A
:PROPERTIES:
:CUSTOM_ID: baz zot
:END:
<|>
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("deadbeef" . "/thud.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[./foo/bar quux.org::#baz zot]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[/foo/bar quux.org::#baz zot]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[/foo/bar quux.org::#baz zot]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[./foo/bar quux.org::#baz zot]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")))

(hyperdrive-test-org-link-deftest same-drive-different-path-on-heading-no-custom-id
  :store-body "
* Heading A
<|>
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("deadbeef" . "/thud.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[./foo/bar quux.org::*Heading A]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[/foo/bar quux.org::*Heading A]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[/foo/bar quux.org::*Heading A]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[./foo/bar quux.org::*Heading A]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")))

(hyperdrive-test-org-link-deftest different-drive-same-path-before-heading
  :store-body "<|>
* Heading A
:PROPERTIES:
:CUSTOM_ID: example ID
:END:
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("fredbeef" . "/foo/bar quux.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")))

(hyperdrive-test-org-link-deftest different-drive-same-path-on-heading-with-custom-id
  :store-body "
* Heading A
:PROPERTIES:
:CUSTOM_ID: baz zot
:END:
<|>
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("fredbeef" . "/foo/bar quux.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")))

(hyperdrive-test-org-link-deftest different-drive-same-path-on-heading-no-custom-id
  :store-body "
* Heading A
<|>
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("fredbeef" . "/foo/bar quux.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")))

(hyperdrive-test-org-link-deftest different-drive-different-path-before-heading
  :store-body "<|>
* Heading A
:PROPERTIES:
:CUSTOM_ID: example ID
:END:
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("fredbeef" . "/thud.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org]]")))

(hyperdrive-test-org-link-deftest different-drive-different-path-on-heading-with-custom-id
  :store-body "
* Heading A
:PROPERTIES:
:CUSTOM_ID: baz zot
:END:
<|>
* Heading B"
  :store-from '("deadbeef" . "/foo/bar quux.org")
  :insert-into '("fredbeef" . "/thud.org")
  :results (( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")
            ( :let ((org-link-file-path-type 'relative)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")
            ( :let ((org-link-file-path-type 'absolute)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")
            ( :let ((org-link-file-path-type 'noabbrev)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")

            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url nil))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")
            ( :let ((org-link-file-path-type 'adaptive)
                    (hyperdrive-org-link-full-url t))
              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot]]")))

(hyperdrive-test-org-link-deftest different-drive-different-path-on-heading-no-custom-id
                                  :store-body "
* Heading A
<|>
* Heading B"
                                  :store-from '("deadbeef" . "/foo/bar quux.org")
                                  :insert-into '("fredbeef" . "/thud.org")
                                  :results (( :let ((org-link-file-path-type 'relative)
                                                    (hyperdrive-org-link-full-url nil))
                                              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")
                                            ( :let ((org-link-file-path-type 'relative)
                                                    (hyperdrive-org-link-full-url t))
                                              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

                                            ( :let ((org-link-file-path-type 'absolute)
                                                    (hyperdrive-org-link-full-url nil))
                                              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")
                                            ( :let ((org-link-file-path-type 'absolute)
                                                    (hyperdrive-org-link-full-url t))
                                              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

                                            ( :let ((org-link-file-path-type 'noabbrev)
                                                    (hyperdrive-org-link-full-url nil))
                                              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")
                                            ( :let ((org-link-file-path-type 'noabbrev)
                                                    (hyperdrive-org-link-full-url t))
                                              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")

                                            ( :let ((org-link-file-path-type 'adaptive)
                                                    (hyperdrive-org-link-full-url nil))
                                              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")
                                            ( :let ((org-link-file-path-type 'adaptive)
                                                    (hyperdrive-org-link-full-url t))
                                              :result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A]]")))

;;     * ~CUSTOM_ID~ :: e.g.
;;       ~hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot~, which decodes to ~hyper://deadbeef/foo/bar quux.org#::#baz zot~

;;     * Heading text search option :: With or without ~*~ (actually [[elisp:(rx "*" (0+ space))]]) prefix, e.g.

;;       - ~hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A~, which decodes to ~hyper://deadbeef/foo/bar quux.org#::*Heading A~
;;       - ~hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2A%20%20Heading%20A~, which decodes to ~hyper://deadbeef/foo/bar quux.org#::*  Heading A~
;;       - ~hyper://deadbeef/foo/bar%20quux.org#%3A%3AHeading%20A~, which decodes to ~hyper://deadbeef/foo/bar quux.org#::Heading A~

;;   - Without protocol prefix

;;     This link type must be surrounded by brackets.  It has no
;;     URL-encoding in any part. It may or may not contain a path:

;;     + With path :: A link pointing to a file at a path, starting with ~/~ or ~.~, with or without search option:

;;       - No search option :: ~[[/foo/bar quux.org]]~

;;       - ~CUSTOM_ID~ :: e.g. ~[[/foo/bar quux.org::#CUSTOM_ID]]~

;;       - Heading text search option :: With or without ~*~ (actually [[elisp:(rx "*" (0+ space))]]) prefix, e.g.

;;         + ~[[/foo/bar quux.org::*Heading A]]~
;;         + ~[[/foo/bar quux.org::*  Heading A]]~
;;         + ~[[/foo/bar quux.org::Heading A]]~

;;     + Without path :: A link pointing to a heading in the same file with search option alone:

;;       - ~CUSTOM_ID~ :: e.g. ~#CUSTOM_ID~

;;       - Heading text search option :: With or without ~*~ (actually [[elisp:(rx "*" (0+ space))]]) prefix, e.g.

;;         + ~*Heading A~
;;         + ~*  Heading A~
;;         + ~Heading A~

