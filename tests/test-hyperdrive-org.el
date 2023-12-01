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

(require 'hyperdrive)
(require 'hyperdrive-org)

(defvar org-id-link-to-org-use-id)

;;;; Tests

;;;;; Scenarios

(defvar h/test-org-store-link-scenarios
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

- \\+`:public-key'
- \\+`:path'
- \\+`:content'
- \\+`:url'
- \\+`:desc'")

;;;;; Store links

(cl-defun h/test-org-store-link (contents &key public-key path)
  "Return stored link to entry with PUBLIC-KEY, PATH, and CONTENTS.
Point is indicated by ★."
  (declare (indent defun))
  (let ((entry (he/create
                :hyperdrive (h/create :public-key public-key)
                :path path))
        org-id-link-to-org-use-id org-stored-links)
    (with-temp-buffer
      (insert contents)
      ;; TODO: Initialize this buffer only once for this file's tests.
      (org-mode)
      (h/mode)
      (setq-local h/current-entry entry)
      (goto-char (point-min))
      (search-forward "★")
      (org-store-link nil 'interactive))
    org-stored-links))

(defmacro h/test-org-store-link-deftest (scenario)
  "Test scenario in `hyperdrive-test-org-store-link-scenarios'."
  (let ((test-name (intern
                    (format "hyperdrive-test-org-store-link/%s" scenario))))
    `(ert-deftest ,test-name ()
       (pcase-let* (((map :public-key :path :content
                          (:url expected-url) (:desc expected-desc))
                     ;; TODO: Is there a better syntax that explicit `quote'?
                     (alist-get (quote ,scenario)
                                h/test-org-store-link-scenarios))
                    (`((,got-url ,got-desc))
                     (h/test-org-store-link content
                       :public-key public-key :path path)))
         (should (string= expected-url got-url))
         (should (string= expected-desc got-desc))))))

;; TODO: Loop through `h/test-org-store-link-scenarios'?
(h/test-org-store-link-deftest org-mode-before-heading)
(h/test-org-store-link-deftest org-mode-on-heading-with-custom-id)
(h/test-org-store-link-deftest org-mode-on-heading-no-custom-id)

;;;;; Insert links

(cl-defun h/test-org-entry-create (&key public-key path)
  (he/create
   :hyperdrive (h/create :public-key public-key)
   :path path))

(cl-defun h/test-org-insert-link-string (scenario &key public-key path)
  "Return link for SCENARIO inserted into entry with PUBLIC-KEY and PATH."
  (declare (indent defun))
  (pcase-let (((map :url :desc) (alist-get scenario h/test-org-store-link-scenarios)))
    (with-temp-buffer
      ;; TODO: Initialize this buffer only once for this file's tests.
      (org-mode)
      (h/mode)
      (setq-local h/current-entry
                  (h/test-org-entry-create :public-key public-key :path path))
      (org-insert-link nil url desc)
      (buffer-string))))

(cl-defmacro h/test-org-insert-link-deftest (name &key public-key path results)
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
                                    (h/test-org-insert-link-string ',scenario
                                      :public-key ,public-key :path ,path)))))
              body-forms)))
    `(progn ,@body-forms)))

;;;;;; Insert shorthand links

(h/test-org-insert-link-deftest org-mode-before-heading/same-drive-same-path
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

(h/test-org-insert-link-deftest org-mode-on-heading-with-custom-id/same-drive-same-path
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

(h/test-org-insert-link-deftest org-mode-on-heading-no-custom-id/same-drive-same-path
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

(h/test-org-insert-link-deftest org-mode-before-heading/same-drive-different-path
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

(h/test-org-insert-link-deftest org-mode-on-heading-with-custom-id/same-drive-different-path
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

(h/test-org-insert-link-deftest org-mode-on-heading-no-custom-id/same-drive-different-path
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

;;;;;; Insert full "hyper://" links

;; Testing a different drive should stand in for testing
;; `h/org-link-full-url' as well as insertion in
;; non-hyperdrive buffers, since all of these cases cause
;; `h/org--insert-link-after-advice' to do nothing.

(h/test-org-insert-link-deftest org-mode-before-heading/different-drive
  :public-key "fredbeef"
  :path "/thud.org"
  :results ((:result "[[hyper://deadbeef/foo/bar%20quux.org]]")))

(h/test-org-insert-link-deftest org-mode-on-heading-with-custom-id/different-drive
  :public-key "fredbeef"
  :path "/thud.org"
  :results ((:result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%23baz%20zot][Heading A]]")))

(h/test-org-insert-link-deftest org-mode-on-heading-no-custom-id/different-drive
  :public-key "fredbeef"
  :path "/thud.org"
  :results
  ((:result "[[hyper://deadbeef/foo/bar%20quux.org#%3A%3A%2AHeading%20A][Heading A]]")))

;;;; Parse relative/absolute link into entry tests

;; Neither full "hyper://"-prefixed URLs, nor links which are only search
;; options, are handled by `h/org--link-entry-at-point'.

(defmacro h/org-test-link-parse-deftest (name current-entry link parsed-entry)
  (declare (indent defun))
  (let ((test-name (intern (format "hyperdrive-test-org-parse-link/%s" name))))
    `(ert-deftest ,test-name ()
       (let ((h/current-entry ,current-entry))
         (with-temp-buffer
           ;; FIXME: Use persistent buffer for performance.
           (org-mode)
           (erase-buffer)
           (insert ,link)
           (goto-char (point-min))
           (should
            (he/equal-p ,parsed-entry (h/org--link-entry-at-point))))))))

(h/org-test-link-parse-deftest absolute/without-search-option
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org")
  "[[/foo/bar quux.org]]"
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org"))

(h/org-test-link-parse-deftest parent/without-search-option
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org")
  "[[../foo/bar quux.org]]"
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org"))

(h/org-test-link-parse-deftest sibling/without-search-option
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org")
  "[[./bar quux.org]]"
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org"))

(h/org-test-link-parse-deftest sibling/with-heading-text-search-option
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org")
  "[[./bar quux.org::Heading A]]"
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org"
   :etc '((target . "Heading A"))))

(h/org-test-link-parse-deftest sibling/with-heading-text*-search-option
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org")
  "[[./bar quux.org::*Heading A]]"
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org"
   :etc '((target . "*Heading A"))))

(h/org-test-link-parse-deftest sibling/with-custom-id-search-option
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org")
  "[[./bar quux.org::#baz zot]]"
  (he/create
   :hyperdrive (h/create :public-key "deadbeef")
   :path "/foo/bar quux.org"
   :etc '((target . "#baz zot"))))

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; test-hyperdrive-org.el ends here
