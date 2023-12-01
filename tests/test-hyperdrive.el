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

(require 'hyperdrive)

;;;; Magic constants

(defconst test-hyperdrive-public-key
  "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

;;;; Utilities

(defmacro h/deftest (name &rest args)
  (declare (indent defun))
  (let ((name (intern (concat "hyperdrive-" (symbol-name name)))))
    `(cl-macrolet ((make-url (&rest args)
                     `(concat "hyper://" test-hyperdrive-public-key ,@args)))
       (ert-deftest ,name () ,@args))))

;;;; Tests

(h/deftest url-entry--names-and-paths ()
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (h/url-entry (make-url ""))))
    (should (equal name "/"))
    (should (equal path "/")))
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (h/url-entry (make-url "/"))))
    (should (equal name "/"))
    (should (equal path "/")))
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (h/url-entry (make-url "/name-without-spaces"))))
    (should (equal name "name-without-spaces"))
    (should (equal path "/name-without-spaces")))
  ;; TODO: Consider testing unhexified filename in URL.
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (h/url-entry (make-url (h//url-hexify-string "/name with spaces")))))
    (should (equal name "name with spaces"))
    (should (equal path "/name with spaces")))
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (h/url-entry (make-url "/subdir/"))))
    (should (equal name "subdir/"))
    (should (equal path "/subdir/")))
  (pcase-let (((cl-struct hyperdrive-entry name path)
               (h/url-entry (make-url "/subdir/with-file"))))
    (should (equal name "with-file"))
    (should (equal path "/subdir/with-file"))))

(h/deftest url-entry--version ()
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (h/url-entry (make-url "/$/version/42"))))
    (should (equal name "/"))
    (should (equal path "/"))
    (should (equal 42 version)))
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (h/url-entry (make-url "/$/version/42/"))))
    (should (equal name "/"))
    (should (equal path "/"))
    (should (equal 42 version)))
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (h/url-entry (make-url "/$/version/42/name-without-spaces"))))
    (should (equal name "name-without-spaces"))
    (should (equal path "/name-without-spaces"))
    (should (equal 42 version)))
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (h/url-entry (make-url "/$/version/42/subdir/"))))
    (should (equal name "subdir/"))
    (should (equal path "/subdir/"))
    (should (equal 42 version)))
  (pcase-let (((cl-struct hyperdrive-entry name path version)
               (h/url-entry (make-url "/$/version/42/subdir/with-file"))))
    (should (equal name "with-file"))
    (should (equal path "/subdir/with-file"))
    (should (equal 42 version))))

(h/deftest url-entry--makes-hyperdrive ()
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive)
                (h/url-entry (make-url (h//url-hexify-string "/subdir/with-file"))))
               ((cl-struct hyperdrive public-key) hyperdrive))
    (should (equal public-key test-hyperdrive-public-key))))

(h/deftest entry-url-round-trip ()

  (let ((url (he/url (h/url-entry (make-url "")))))
    (should (equal url (format "hyper://%s/" test-hyperdrive-public-key))))

  (let ((url (he/url (h/url-entry (make-url "/")))))
    (should (equal url (format "hyper://%s/" test-hyperdrive-public-key))))

  (let ((url (he/url (h/url-entry (make-url "/name-without-spaces")))))
    (should (equal url (format "hyper://%s/name-without-spaces"
                               test-hyperdrive-public-key))))

  (let ((url (he/url (h/url-entry (make-url "/name%20without%20spaces")))))
    (should (equal url (format "hyper://%s/name%%20without%%20spaces"
                               test-hyperdrive-public-key))))

  (let ((url (he/url (h/url-entry (make-url "/name%20without%20spaces/subdir")))))
    (should (equal url (format "hyper://%s/name%%20without%%20spaces/subdir"
                               test-hyperdrive-public-key)))))

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; test-hyperdrive.el ends here
