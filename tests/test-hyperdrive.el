;;; test-hyperdrive.el --- Tests for Hyperdrive.el  -*- lexical-binding: t; -*-

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

;; TODO: Consider having a "round-trip" test that makes a new
;; hyperdrive, writes files of various names, then reads the
;; hyperdrive back and ensures that the entries names and paths are
;; correctly encoded/decoded.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'ert)
(require 'pcase)
(require 'url)

(require 'with-simulated-input)

(require 'hyperdrive)

;;;; Magic constants

(defconst test-hyperdrive-public-key
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

;;;; Utilities

(defmacro hyperdrive-deftest (name &rest args)
  (declare (indent defun))
  (let ((name (intern (concat "hyperdrive-" (symbol-name name)))))
    `(cl-macrolet ((make-url
                     (&rest args) `(concat "hyper://" test-hyperdrive-public-key ,@args))
                   (hexify (string) `(url-hexify-string ,string (cons ?/ url-unreserved-chars))))
       (ert-deftest ,name () ,@args))))

;;;; Tests

(hyperdrive-deftest url-entry--names-and-paths ()
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (hyperdrive-url-entry (make-url ""))))
    (should (equal name "/"))
    (should (equal path "/")))
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (hyperdrive-url-entry (make-url "/"))))
    (should (equal name "/"))
    (should (equal path "/")))
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (hyperdrive-url-entry (make-url "/name-without-spaces"))))
    (should (equal name "name-without-spaces"))
    (should (equal path "/name-without-spaces")))
  ;; TODO: Consider testing unhexified filename in URL.
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (hyperdrive-url-entry (make-url (hexify "/name with spaces")))))
    (should (equal name "name with spaces"))
    (should (equal path "/name%20with%20spaces")))
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (hyperdrive-url-entry (make-url "/subdir/"))))
    (should (equal name "subdir/"))
    (should (equal path "/subdir/")))
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (hyperdrive-url-entry (make-url "/subdir/with-file"))))
    (should (equal name "with-file"))
    (should (equal path "/subdir/with-file"))))

(hyperdrive-deftest url-entry--version ()
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (hyperdrive-url-entry (make-url "/$/version/42"))))
    (should (equal name "/"))
    (should (equal path "/"))
    (should (equal 42 version)))
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (hyperdrive-url-entry (make-url "/$/version/42/"))))
    (should (equal name "/"))
    (should (equal path "/"))
    (should (equal 42 version)))
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (hyperdrive-url-entry (make-url "/$/version/42/name-without-spaces"))))
    (should (equal name "name-without-spaces"))
    (should (equal path "/name-without-spaces"))
    (should (equal 42 version)))
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (hyperdrive-url-entry (make-url "/$/version/42/subdir/"))))
    (should (equal name "subdir/"))
    (should (equal path "/subdir/"))
    (should (equal 42 version)))
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (hyperdrive-url-entry (make-url "/$/version/42/subdir/with-file"))))
    (should (equal name "with-file"))
    (should (equal path "/subdir/with-file"))
    (should (equal 42 version))))

(hyperdrive-deftest url-entry--makes-hyperdrive ()
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive)
                (hyperdrive-url-entry (make-url (hexify "/subdir/with-file"))))
               ((cl-struct hyperdrive public-key) hyperdrive))
    (should (equal public-key test-hyperdrive-public-key))))

;;;; Link testing

;;;;; Opening links

(require 'hyperdrive-org)

;;;;;; URL links (i.e. "hyper://"-prefixed)

(defun hyperdrive-test-org-url-link (url)
  "Return the URL that would be opened by `hyperdrive-open' for URL."
  (let (called-with-url)
    (with-temp-buffer
      (org-mode)
      (insert url)
      (goto-char 1)
      (cl-letf (((symbol-function 'hyperdrive-open)
                 (lambda (entry &rest _)
                   (setf called-with-url (hyperdrive-entry-url entry)))))
        (org-open-at-point)))
    called-with-url))

(ert-deftest hyperdrive-org-link-with-protocol-no-target ()
  (let ((links '(("hyper://public_key/links%20test.org" . "hyper://public_key/links%20test.org")
                 ("[[hyper://public_key/links%20test.org]]" . "hyper://public_key/links%20test.org"))))
    (dolist (link links)
      (should (equal (hyperdrive-test-org-url-link (car link)) (cdr link))))))

(ert-deftest hyperdrive-org-link-with-protocol-with-target ()
  (let ((links '(("[[hyper://public_key/links%20test.org#Heading%20A]]"
                  . "hyper://public_key/links%20test.org#Heading%20A")
                 ("hyper://public_key/links%20test.org#Heading%20A"
                  . "hyper://public_key/links%20test.org#Heading%20A"))))
    (dolist (link links)
      (should (equal (hyperdrive-test-org-url-link (car link)) (cdr link))))))

;;;;;; Org links (i.e. not "hyper://"-prefixed)

(defun hyperdrive-test-org-link (link)
  ;; FIXME: Docstring.
  "Return the URL that would be opened by `hyperdrive-open' for LINK.
LINK is an Org link as a string."
  (let (called-with-url)
    (with-temp-buffer
      (org-mode)
      (hyperdrive-mode)
      (insert link)
      (goto-char 1)
      (cl-letf (((symbol-function 'hyperdrive-open)
                 (lambda (entry &rest _)
                   (setf called-with-url (hyperdrive-entry-url entry)))))
        (org-open-at-point)))
    called-with-url))

;; (ert-deftest hyperdrive-org-link-without-protocol-without-target-filename-with-spaces ()
;;   (let ((links '(("[[links test.org]]"))))
;;     (dolist (link links)
;;       (should (equal )))))

;; (ert-deftest hyperdrive-org-link-without-protocol-without-file-with-asterisk-target ()
;;   (let ((links '(("[[*Heading A]]"))))
;;     (dolist (link links)
;;       (should (equal (hyperdrive-test-org-link (car link)) "WHAT")))))

;;;;; Inserting links

(cl-defun hyperdrive-test-org-link-roundtrip
    (contents &key store-from insert-into)
  (let ((org-id-link-to-org-use-id nil)
        (default-directory "/")
        (org-link-file-path-type
         (lambda (path)
           (replace-regexp-in-string (rx bos (optional "file:")
                                         "/hyper:/")
                                     "hyper://" path)))
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

;;;;;; Test cases

(ert-deftest hyperdrive-link-no-protocol-no-path-same-drive-same-file-custom-id ()
  (should
   (equal "[[#example ID]]"
          (hyperdrive-test-org-link-roundtrip
           "
* Heading A
:PROPERTIES:
:CUSTOM_ID: example ID
:END:
<|>
* Heading B"
           :store-from '("deadbeef" . "/foo/bar.org")
           :insert-into '("deadbeef" . "/foo/bar.org")))))

(ert-deftest hyperdrive-link-same-drive-different-file-before-heading ()
  "Linking to a file (before the first heading) and on same drive."
  (should
   (equal "[[/foo/bar.org]]"
          (hyperdrive-test-org-link-roundtrip
           "<|>
* Heading A
* Heading B"
           :store-from '("deadbeef" . "/foo/bar.org")
           :insert-into '("deadbeef" . "/foo/zot.org")))))

(ert-deftest hyperdrive-link-same-drive-same-file-in-heading-without-custom-id ()
  "Linking to a heading within the same file (and on same drive)."
  (should
   (equal "[[*Heading A]]"
          (hyperdrive-test-org-link-roundtrip
           "* Heading A
<|>
* Heading B"
           :store-from '("deadbeef" . "/foo/bar.org")
           :insert-into '("deadbeef" . "/foo/bar.org")))))

(ert-deftest hyperdrive-link-heading-within-drive ()
  "Linking to a heading within the same drive but different file.")

;;;;;;; With protocol

;; These links will look the same regardless of hyperdrive or path.

(ert-deftest hyperdrive-link-different-drive-with-custom-id ()
  (should
   (equal "[[hyper://deadbeef/foo/bar.org#%3A%3A%23example%20ID]]"
          (hyperdrive-test-org-link-roundtrip
           "
* Heading A
:PROPERTIES:
:CUSTOM_ID: example ID
:END:
<|>
* Heading B"
           :store-from '("deadbeef" . "/foo/bar.org")
           :insert-into '("fredbeef" . "/foo/bar.org")))))

;; (hyperdrive-test-org-link-roundtrip
;;  "<|>
;; * Heading A
;; * Heading B")
;; "[[hyper://public-key/foo/bar]]"

;; (hyperdrive-test-org-link-roundtrip
;;  "* Heading A
;; <|>
;; * Heading B")
;; "[[hyper://public-key/foo/bar#Heading%20A][Heading A]]"

;; (hyperdrive-test-org-link-roundtrip
;;  "* Heading A
;; :PROPERTIES:
;; :ID: deadbeef
;; :END:
;; <|>
;; * Heading B")
;; "[[hyper://public-key/foo/bar#deadbeef][Heading A]]"

;; (hyperdrive-test-org-link-roundtrip
;;  "* Heading A
;; :PROPERTIES:
;; :CUSTOM_ID: custom-id
;; :END:
;; <|>
;; * Heading B")
;; "[[hyper://public-key/foo/bar#custom-id][Heading A]]"


;; "hyper://public-key/foo/bar#deadbeef"
