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
(require 'map)
(require 'pcase)
(require 'url-util)

(require 'compat)
(require 'persist)
(require 'plz)

;;;; Declarations

(declare-function hyperdrive-mode "hyperdrive")
(declare-function hyperdrive-open-url "hyperdrive")

;;;; Structs

(cl-defstruct hyperdrive-entry
  "Represents an entry in a hyperdrive."
  (hyperdrive nil :documentation "The entry's hyperdrive.")
  ;; (url nil :documentation "Canonical URL to entry.")
  ;; Rather than storing just the path and making a function to return
  ;; the name, we store the name as-is because, for one thing, the name
  ;; could theoretically contain a slash, and `file-name-nondirectory'
  ;; would return the wrong value in that case.
  (name nil :documentation "Filename of entry (excluding leading slash).")
  (path nil :documentation "Path (including leading slash).")
  (headers nil :documentation "HTTP headers from request.")
  (modified nil :documentation "Last modified time.")
  (size nil :documentation "Size of file.")
  (etag nil :documentation "Entry's Etag.")
  (type nil :documentation "MIME type of the entry.")
  (etc nil :documentation "Alist for extra data about the entry."))

(cl-defstruct hyperdrive
  "Represents a hyperdrive."
  (public-key nil :documentation "Hyperdrive's public key.")
  (alias nil :documentation "Alias (always and only present for writable hyperdrives).")
  (readablep nil :documentation "Whether the drive is readable.")
  (writablep nil :documentation "Whether the drive is writable."))

(defun hyperdrive-url (hyperdrive)
  "Return a \"hyper://\"-prefixed URL from a HYPERDRIVE struct.
URL does not have a trailing slash, i.e., \"hyper://PUBLIC-KEY\"."
  (concat "hyper://" (hyperdrive-public-key hyperdrive)))

(defun hyperdrive-entry-url (entry)
  "Return ENTRY's URL."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
               (url (hyperdrive-url hyperdrive))
               (encoded-path (url-hexify-string
                              path (cons ?/ url-unreserved-chars))))
    (concat url encoded-path)))

;;;; Variables

(defvar hyperdrive-timestamp-format-string nil)

(defvar hyperdrive-current-entry)
(defvar hyperdrive-hyper-gateway-port)
(defvar hyperdrive-hyperdrives)

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

;;;; API

;; These functions take a URL argument, not a hyperdrive-entry struct.

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

(cl-defun hyperdrive--write (url &key body then else)
  "Save BODY (a string) to hyperdrive URL.
THEN and ELSE are passed to `hyperdrive-api', which see."
  (declare (indent defun))
  (hyperdrive-api 'put url
    ;; TODO: Investigate whether we should use 'text body type for text buffers.
    :body-type 'binary
    ;; TODO: Make plz accept a buffer as the body.
    :body body
    :then then :else else))

(defun hyperdrive--parent (url)
  ;; TODO: Maybe rename this?
  "Return parent URL for URL.
If already at top-level directory, return nil."
  (let ((parent-url (file-name-directory (directory-file-name url))))
    (unless (equal parent-url hyperdrive--hyper-prefix)
      parent-url)))

;; (defun hyperdrive--readable-p (url)
;;   "Return non-nil if URL is readable.
;; That is, it is known to exist and its contents are readable.  A
;; nil return value does not necessarily mean that the hyperdrive
;; does not exist: it may be non-existent, or its contents may be
;; currently inaccessible."
;;   (when-let ((response (hyperdrive-api 'head url :as 'response :else #'ignore)))
;;     (pcase-let* (((cl-struct plz-response headers) response)
;;                  ((map etag) headers))
;;       ;; TODO: If hyperdrive-gateway is changed to return HTTP 204 for
;;       ;; hyperdrive that's never had content, update this.  See:
;;       ;; <https://github.com/RangerMauve/hypercore-fetch/issues/57>.
;;       (>= (string-to-number etag) 1))))

(defun hyperdrive-url-entry (url)
  "Return entry for URL.
Set entry's hyperdrive slot to persisted hyperdrive if it exists."
  (pcase-let* (((cl-struct url (host public-key) (filename path) target)
                (url-generic-parse-url url))
               ;; TODO: For now, no other function besides `hyperdrive-url-entry' calls
               ;; `make-hyperdrive', but perhaps it would be good to add a function which wraps
               ;; `make-hyperdrive' and returns either an existing hyperdrive or a new one?
               (hyperdrive (or (gethash public-key hyperdrive-hyperdrives)
                               (make-hyperdrive :public-key public-key)))
               (etc (when target
                      (list (cons 'target target)))))
    ;; e.g. for hyper://PUBLIC-KEY/path/to/basename, we do:
    ;; :path "/path/to/basename" :name "basename"
    (make-hyperdrive-entry :hyperdrive hyperdrive
                           :path (if (string-empty-p path) "/" path)
                           ;; TODO: Verify that this is the right for directories.
                           :name (file-name-nondirectory path)
                           :etc etc)))
;;;; Entries

;; These functions take a hyperdrive-entry struct argument, not a URL.

;; (defun hyperdrive-entry-equal (a b)
;;   "Return non-nil if hyperdrive entries A and B are equal."
;;   (pcase-let (((cl-struct hyperdrive-entry (path a-path)
;;                           (hyperdrive (cl-struct hyperdrive (public-key a-key))))
;;                a)
;;               ((cl-struct hyperdrive-entry (path b-path)
;;                           (hyperdrive (cl-struct hyperdrive (public-key b-key))) )
;;                b))
;;     (and (equal a-path b-path)
;;          (equal a-key b-key))))

(cl-defun hyperdrive-fill
    (entry &key then
           (else (lambda (plz-error)
                   ;; FIXME: Use a message instead of a warning for
                   ;; now, because the 404 errors for filenames with
                   ;; spaces are annoying as warnings.
                   (hyperdrive-message (format "hyperdrive-fill: error: %S" plz-error) )
                   ;; (display-warning 'hyperdrive
                   ;;                  (format "hyperdrive-fill: error: %S" plz-error))
                   )))
  "Fill ENTRY's metadata and call THEN.
If request fails, call ELSE (which is passed to `hyperdrive-api',
which see."
  (declare (indent defun))
  (hyperdrive-api 'head (hyperdrive-entry-url entry)
    :as 'response
    :then (lambda (response)
            (funcall then (hyperdrive--fill entry (plz-response-headers response))))
    :else else
    :noquery t))

(defun hyperdrive--fill (entry headers)
  "Fill ENTRY's slot from HEADERS."
  (pcase-let (((cl-struct hyperdrive-entry name path) entry)
              ((map content-length content-type etag last-modified) headers))
    (unless name
      (setf (hyperdrive-entry-name entry) (string-trim path "/")))
    (when last-modified
      (setf last-modified (encode-time (parse-time-string last-modified))))
    (setf (hyperdrive-entry-size entry) (when content-length
                                          (ignore-errors
                                            (cl-parse-integer content-length)))
          (hyperdrive-entry-type entry) content-type
          (hyperdrive-entry-etag entry) etag
          (hyperdrive-entry-modified entry) last-modified)
    entry))

(cl-defun hyperdrive-delete (entry &key then else)
  "Delete ENTRY, then call THEN.
Call ELSE if request fails."
  (declare (indent defun))
  (hyperdrive-api 'delete (hyperdrive-entry-url entry)
    :then then :else else))

(cl-defun hyperdrive-write (entry &key body then else)
  "Write BODY to hyperdrive ENTRY's URL."
  (declare (indent defun))
  (hyperdrive--write (hyperdrive-entry-url entry)
    :body body :then then :else else))

(cl-defun hyperdrive--format-entry-url
    (entry &key abbreviate-key (with-alias t) (with-protocol t))
  "Return human-readable version of ENTRY's URL.
Return URL formatted like:

  hyper://[ALIAS]/PATH/TO/FILE
  hyper://PUBLIC-KEY/PATH/TO/FILE

If USE-ALIAS, the public-key is replaced with it, when available.
If ABBREVIATE-KEY, the public key is shortened to 6 characters
and an ellipsis.  If WITH-PROTOCOL, \"hyper://\" is prepended.
Entire string has `help-echo' property showing the entry's full
URL."
  ;; TODO: Add user option to disable abbreviating keys.
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
               ((cl-struct hyperdrive public-key alias) hyperdrive)
               (protocol (when with-protocol
                           "hyper://"))
               (host (if (and with-alias alias)
                         (propertize (concat "[" alias "]") 'face 'hyperdrive-alias)
                       (propertize (if abbreviate-key
                                       (concat (substring public-key 0 6) "…")
                                     public-key)
                                   'face 'hyperdrive-public-key))))
    (propertize (concat protocol host path)
                'help-echo (hyperdrive-entry-url entry))))

;;;; Reading from the user

(cl-defun hyperdrive-complete-hyperdrive (&key predicate (prompt "Hyperdrive: "))
  "Return a hyperdrive selected with completion.
If PREDICATE, only offer hyperdrives matching it."
  (let* ((hyperdrives (cl-remove-if-not predicate (hash-table-values hyperdrive-hyperdrives)))
         candidates)
    (dolist (hyperdrive hyperdrives)
      (push (cons (hyperdrive-public-key hyperdrive) hyperdrive) candidates)
      (when-let ((alias (hyperdrive-alias hyperdrive)))
        (push (cons alias hyperdrive) candidates)))
    (alist-get (completing-read prompt (mapcar #'car candidates) nil 'require-match)
               candidates nil nil #'equal)))

(cl-defun hyperdrive-read-entry (&key predicate)
  "Return new hyperdrive entry with path and hyperdrive read from user.
Prompts user for a hyperdrive and signals an error if no
such hyperdrive is known.
If PREDICATE, only offer hyperdrives matching it."
  (let* ((hyperdrive (hyperdrive-complete-hyperdrive :predicate predicate))
         (filename (buffer-file-name))
         (basename (or (when hyperdrive-current-entry
                         (hyperdrive-entry-name hyperdrive-current-entry))
                       (when filename
                         (file-name-nondirectory filename))))
         (default (or basename (buffer-name)))
         (prompt (format "File path [default %S]: " default))
         (path (read-string prompt nil nil default)))
    (unless (hyperdrive-p hyperdrive)
      (user-error "No such hyperdrive: %S.  Use `hyperdrive-new' to create a new one" hyperdrive))
    (make-hyperdrive-entry :hyperdrive hyperdrive
                           :name (file-name-nondirectory path)
                           :path (if (string-prefix-p "/" path)
                                     path
                                   (concat "/" path)))))

(defun hyperdrive-new (alias)
  "Open new hyperdrive for ALIAS."
  (interactive (list (read-string "New hyperdrive alias: ")))
  (let* ((response (with-local-quit
                     (hyperdrive-api 'post (concat "hyper://localhost/?key=" (url-hexify-string alias)))))
         (url (progn
                ;; NOTE: Working around issue in plz whereby the
                ;; stderr process sentinel sometimes leaves "stderr
                ;; finished" garbage in the response body in older
                ;; Emacs versions.  See: <https://github.com/alphapapa/plz.el/issues/23>.
                (string-match (rx bos (group "hyper://" (1+ nonl))) response)
                (match-string 1 response)))
         (hyperdrive (hyperdrive-entry-hyperdrive (hyperdrive-url-entry url))))
    (setf (hyperdrive-alias hyperdrive) alias
          (hyperdrive-writablep hyperdrive) t)
    (hyperdrive-persist hyperdrive)
    (hyperdrive-open-url url)))

(defun hyperdrive-persist (hyperdrive)
  "Persist HYPERDRIVE in `hyperdrive-hyperdrives'."
  (puthash (hyperdrive-public-key hyperdrive) hyperdrive hyperdrive-hyperdrives)
  (persist-save 'hyperdrive-hyperdrives))

;;;; Misc.

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
  (with-current-buffer (get-buffer-create (hyperdrive--format-entry-url entry :abbreviate-key t))
    (hyperdrive-mode)
    (setq-local hyperdrive-current-entry entry)
    (current-buffer)))

(defun hyperdrive--entry-directory-p (entry)
  "Return non-nil if ENTRY is a directory."
  (string-suffix-p "/" (hyperdrive-entry-url entry)))

(defun hyperdrive-message (message &rest args)
  "Call `message' prefixing MESSAGE with \"Hyperdrive:\"."
  (apply #'message (concat "Hyperdrive: " message) args))

(provide 'hyperdrive-lib)
;;; hyperdrive-lib.el ends here
