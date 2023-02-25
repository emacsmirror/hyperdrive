;;; hyperdrive-lib.el --- Library functions and structures  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USHIN, Inc.

;; Author: Adam Porter <adam@alphapapa.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)

(require 'compat)
(require 'persist)
(require 'plz)

;;;; Structs

(cl-defstruct hyperdrive-directory
  "Represents a directory in a hyperdrive."
  ;; FIXME: Do we even need this struct?  Or will we need it later?
  ;; TODO: Add URL slot.
  (headers nil :documentation "HTTP headers from request.")
  (modified nil :documentation "Last modified time.")
  (url nil :documentation "URL returned by gateway.")
  (entries nil :documentation "Entries in the directory."))

(cl-defstruct hyperdrive-entry
  "Represents an entry in a hyperdrive."
  (url nil :documentation "Canonical URL to entry.")
  (parent-url nil :documentation "URL to entry's parent directory (i.e. does not include name).")
  (name nil :documentation "Name of entry.")
  (headers nil :documentation "HTTP headers from request.")
  (modified nil :documentation "Last modified time.")
  (size nil :documentation "Size of file.")
  (etag nil :documentation "Entry's Etag.")
  (type nil :documentation "MIME type of the entry.")
  (etc nil :documentation "Alist for extra data about the entry."))

;;;; Variables

(defvar hyperdrive-hyper-gateway-port)
(defvar hyperdrive--namespaces)

(eval-and-compile
  (defconst hyperdrive--hyper-prefix "hyper://"
    "Hyperdrive URL prefix."))

(defconst hyperdrive--public-key-re
  (rx (eval hyperdrive--hyper-prefix) (group (= 52 alphanumeric)))
  "Regex to match \"hyper://\" + public key.

Capture group matches public key.")

(defconst hyperdrive--version-re
  (rx (eval hyperdrive--hyper-prefix)
      (one-or-more alnum)
      (group "+" (one-or-more num)))
  "Regex to match \"hyper://\" + public key or alias + version number.

Capture group matches version number.")

(defconst hyperdrive-metadata-filename ".ushin.json"
  "Location of hyperdrive.el metadata inside hyperdrive.")

;;;; API

(cl-defun hyperdrive-api (method url &rest rest)
  "Make hyperdrive API request.
Calls `hyperdrive--httpify-url' to convert HYPER-URL starting
with `hyperdrive--hyper-prefix' to a URL starting with
\"http://localhost:4973/hyper/\" (assuming that
`hyper-gateway-port' is \"4973\").

The remaining arguments are passed to `plz', which see."
  (declare (indent defun))
  (apply #'plz method (hyperdrive--httpify-url url) rest))

(defun hyperdrive--httpify-url (url)
  "Return localhost HTTP URL for HYPER-URL."
  (concat "http://localhost:" (number-to-string hyperdrive-hyper-gateway-port) "/hyper/"
          (substring url (length hyperdrive--hyper-prefix))))

(defun hyperdrive--parent-url (entry)
  "Return URL of ENTRY's parent.
If already at top-level directory, return nil."
  (pcase-let* (((cl-struct hyperdrive-entry url) entry)
               (parent-url (file-name-directory (directory-file-name url))))
    (unless (equal parent-url hyperdrive--hyper-prefix)
      parent-url)))

(defun hyperdrive--format-url (url)
  "Return human-readable version of URL where the public-key is
replaced with its local alias or public name.

If no alias or name exists, return URL."
  (let ((display-name (or
                       (hyperdrive--alias url)
                       (alist-get 'name (hyperdrive-metadata url))))
        (public-key (hyperdrive--extract-public-key url)))
    (if display-name
        (replace-regexp-in-string
         (regexp-quote (concat hyperdrive--hyper-prefix public-key))
         (concat (substring public-key 0 6) "<" display-name ">" ":")
         url)
      url)))

;;;; Entries

(defun hyperdrive-fill-entry (entry &optional then)
  "Fill ENTRY's metadata and call THEN."
  (declare (indent defun))
  (hyperdrive-api 'head (hyperdrive-entry-url entry)
    :as 'response
    :then (lambda (response)
            (funcall then (hyperdrive--fill-entry entry (plz-response-headers response))))
    :else (lambda (plz-error)
            (display-warning 'hyperdrive (format "hyperdrive-fill-entry: error: %S" plz-error)))))

(defun hyperdrive--fill-entry (entry headers)
  "Fill ENTRY's slot from HEADERS."
  (pcase-let (((map content-length content-type etag last-modified) headers))
    (setf (hyperdrive-entry-size entry) (when content-length
                                          (ignore-errors
                                            (cl-parse-integer content-length)))
          (hyperdrive-entry-type entry) content-type
          (hyperdrive-entry-etag entry) etag
          (hyperdrive-entry-modified entry) last-modified)
    entry))

;;;; Misc.

;; TODO: Tidy these further.

(defun hyperdrive--get-buffer-create (entry)
  "Return buffer for ENTRY.
Names buffer, sets `buffer-file-name' and
`hyperdrive-current-entry'.

This function helps prevent duplicate `hyperdrive-mode' buffers
by ensuring that buffer names always use the namespace alias
corresponding to URL if possible.

In other words, this avoids the situation where a buffer called
\"foo:/\" and another called \"hyper://<public key for foo>/\"
both point to the same content."
  (pcase-let (((cl-struct hyperdrive-entry url) entry))
    (with-current-buffer (get-buffer-create (hyperdrive--format-url url))
      (setq-local hyperdrive-current-entry entry)
      (current-buffer))))

(defun hyperdrive--public-key-by-alias (alias)
  "Return public key corresponding to ALIAS in `hyperdrive--namespaces'."
  (cdr (assoc alias hyperdrive--namespaces)))

(defun hyperdrive--make-hyperdrive-url (public-key raw-path)
  "Return `hyperdrive--hyper-prefix'-prefixed url from PUBLIC-KEY and RAW-PATH.

Path portion of url is URI-encoded."
  (let* ((encoded-portion (url-hexify-string
                           (if (string-prefix-p "/" raw-path) (substring raw-path 1) raw-path)
                           ;; Leave slashes unencoded.  See <https://todo.sr.ht/~ushin/ushin/39>.
                           (cons ?/ url-unreserved-chars)))
         (path (concat "/" encoded-portion)))
    (concat hyperdrive--hyper-prefix public-key path)))

(defun hyperdrive--extract-public-key (string)
  "Extract public-key from STRING using `hyperdrive--public-key-re'."
  (when (string-match hyperdrive--public-key-re string)
    (match-string 1 string)))

(defun hyperdrive--alias (url)
  "Return alias corresponding to public key in URL. Otherwise, return URL."
  (car (rassoc (hyperdrive--extract-public-key url) hyperdrive--namespaces)))

(defun hyperdrive-metadata (url)
  "Return alist converted from JSON file at
`hyperdrive-metadata-filename' in hyperdrive for URL."
  (let ((json-array-type 'list)
        (url (progn
               (string-match hyperdrive--public-key-re url)
               (match-string 0 url))))
    (hyperdrive-api 'get (concat url "/" hyperdrive-metadata-filename) :as #'json-read)))

(defun hyperdrive--extract-path (string)
  "Extract path following public-key from STRING."
  (substring string (+ (length hyperdrive--hyper-prefix)
                       (length (hyperdrive--extract-public-key string)))))

(defun hyperdrive--add-version-to-url (link version)
  "Add VERSION number to url from LINK and (optionally) VERSION.

This function returns a url of the form \"hyper://\" + public-key
+ path or \"hyper://\" + public-key + version number + path,
while urls from hyper-gateway response headers lack version
numbers."
  (concat hyperdrive--hyper-prefix
          (hyperdrive--extract-public-key link)
          (and version (concat "+" version))
          (hyperdrive--extract-path link)))

(defun hyperdrive--headers-extract-url (headers)
  "Extract url from response HEADERS.

Returned url does not contain version number."
  (let ((str (alist-get 'link headers)))
    (when (string-match (rx "<" (group (one-or-more anything)) ">")
                        str)
      (match-string 1 str))))

(defun hyperdrive--headers-extract-version (headers)
  "Extract version number (etag) from response HEADERS.

Version number is of type string"
  (alist-get 'etag headers))

(defun hyperdrive--directory-p (url)
  "Return non-nil if url is a directory."
  (string-match "/$" url))

(defun hyperdrive--streamable-p (headers)
  "Return non-nil if response HEADERS indicate that the content is
audio or video which can be streamed with mpv."
  (string-match (rx (or "audio" "video")) (alist-get 'content-type headers)))

(defun hyperdrive--version-match (url)
  "Return non-nil if URL contains a version number."
  (string-match hyperdrive--version-re url))

(provide 'hyperdrive-lib)
;;; hyperdrive-lib.el ends here
