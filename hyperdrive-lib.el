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

(require 'compat)
(require 'persist)
(require 'plz)

;;;; Structs

;; (cl-defstruct hyperdrive-directory
;;   "Represents a directory in a hyperdrive."
;;   ;; FIXME: Do we even need this struct?  Or will we need it later?
;;   ;; TODO: Add URL slot.
;;   (headers nil :documentation "HTTP headers from request.")
;;   (modified nil :documentation "Last modified time.")
;;   (url nil :documentation "URL returned by gateway.")
;;   (entries nil :documentation "Entries in the directory."))

(cl-defstruct hyperdrive-entry
  "Represents an entry in a hyperdrive."
  (hyperdrive nil :documentation "The entry's hyperdrive.")
  ;; (url nil :documentation "Canonical URL to entry.")
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

(defun hyperdrive-url-entry (url)
  "Return entry for URL."
  (pcase-let* (((cl-struct url (host public-key) (filename path) target)
                (url-generic-parse-url url))
               (hyperdrive (make-hyperdrive :public-key public-key))
               (etc (when target
                      (list (cons 'target target)))))
    ;; e.g. for hyper://PUBLIC-KEY/path/to/basename, we do:
    ;; :path "/path/to/basename" :name "basename"
    (make-hyperdrive-entry :hyperdrive hyperdrive
                           :path (if (string-empty-p path) "/" path)
                           ;; TODO: Verify that this is the right for directories.
                           :name (file-name-nondirectory path)
                           :etc etc)))

;;;; Variables

(defvar hyperdrive-timestamp-format-string nil)

(defvar hyperdrive-current-entry)
(defvar hyperdrive-hyper-gateway-port)
(defvar hyperdrive--namespaces)
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

;; (defconst hyperdrive-metadata-filename ".ushin.json"
;;   "Location of hyperdrive.el metadata inside hyperdrive.")

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

(defun hyperdrive--format-url (url)
  "Return human-readable version of URL where the public-key is
replaced with its local alias or public name.

If no alias or name exists, return URL."
  (let ((display-name (or
                       (hyperdrive--alias url)
                       ;; (alist-get 'name (hyperdrive-metadata url))
                       ))
        (public-key (hyperdrive--extract-public-key url)))
    (if display-name
        (replace-regexp-in-string
         (regexp-quote (concat hyperdrive--hyper-prefix public-key))
         (concat (substring public-key 0 6) "<" display-name ">" ":")
         url)
      url)))

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

;;;; Entries

;; These functions take a hyperdrive-entry struct argument, not a URL.

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
    :else else))

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

;;;; Reading from the user

(defun hyperdrive--completing-read-alias ()
  "Return an alias from `hyperdrive--namespaces'.

Prompt user to select an alias, or if only one namespace exists,
select it automatically."
  (if hyperdrive--namespaces
      (if (= 1 (length hyperdrive--namespaces))
          (caar hyperdrive--namespaces)
        (completing-read "Alias: " hyperdrive--namespaces nil t))
    ;; TODO: Prompt user to create namespace here?
    (user-error "No namespace defined. Please run M-x hyperdrive-create-namespace")))

(cl-defun hyperdrive-complete-hyperdrive (&key predicate (prompt "Hyperdrive: "))
  "Return a hyperdrive selected with completion, or the input if nothing matches.
If PREDICATE, only offer hyperdrives matching it."
  ;; TODO: Implement predicate.
  (ignore predicate)
  (let* ((aliases (mapcar #'hyperdrive-alias hyperdrive-hyperdrives))
         (urls (mapcar #'hyperdrive-url hyperdrive-hyperdrives))
         (candidates (append aliases urls))
         (input (completing-read prompt candidates)))
    (or (cl-find-if (lambda (hyperdrive)
                      (or (equal input (hyperdrive-alias hyperdrive))
                          (equal input (hyperdrive-url hyperdrive))
                          ;; In case the user adds a trailing slash.
                          (equal input (concat (hyperdrive-url hyperdrive) "/"))))
                    hyperdrive-hyperdrives)
        input)))

(cl-defun hyperdrive-complete-url (&key (prompt "Hyperdrive alias or URL: "))
  "Return hyperdrive URL selected with completion."
  (let ((selected (hyperdrive-complete-hyperdrive :prompt prompt)))
    (pcase selected
      ((pred hyperdrive-p) (hyperdrive-url selected))
      ((and (pred stringp)
            (guard (string-prefix-p "hyper://" selected)))
       ;; User input a hyperdrive URL: return it.
       selected)
      (_ (user-error "Please select a known hyperdrive or input a hyper:// URL")))))

(defun hyperdrive--read-new-entry ()
  "Return new hyperdrive entry with path and hyperdrive read from user."
  (let* ((hyperdrive (hyperdrive-complete-hyperdrive))
         (filename (buffer-file-name))
         (basename (or (when hyperdrive-current-entry
                         (hyperdrive-entry-name hyperdrive-current-entry))
                       (when filename
                         (file-name-nondirectory filename))))
         (default (or basename (buffer-name)))
         (prompt (format "File path [default %S]: " default))
         (path (read-string prompt nil nil default)))
    (make-hyperdrive-entry :hyperdrive hyperdrive
                           :name (file-name-nondirectory path)
                           :path (if (string-prefix-p "/" path)
                                     path
                                   (concat "/" path)))))

(defun hyperdrive-new (alias)
  "Return new hyperdrive for ALIAS."
  (let* ((url (hyperdrive-api 'post (concat "hyper://localhost/?key=" (url-hexify-string alias))))
         (hyperdrive (hyperdrive-entry-hyperdrive (hyperdrive-url-entry url))))
    (hyperdrive-persist hyperdrive)
    hyperdrive))

(defun hyperdrive-persist (hyperdrive)
  "Persist HYPERDRIVE in `hyperdrive-hyperdrives'."
  (cl-pushnew hyperdrive hyperdrive-hyperdrives :test #'hyperdrive-public-key))

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
  (with-current-buffer (get-buffer-create (hyperdrive--format-url (hyperdrive-entry-url entry)))
    (setq-local hyperdrive-current-entry entry)
    (current-buffer)))

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

;; (defun hyperdrive-metadata (url)
;;   "Return alist converted from JSON file at
;; `hyperdrive-metadata-filename' in hyperdrive for URL."
;;   (let ((json-array-type 'list)
;;         (url (progn
;;                (string-match hyperdrive--public-key-re url)
;;                (match-string 0 url))))
;;     (hyperdrive-api 'get (concat url "/" hyperdrive-metadata-filename) :as #'json-read)))

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

(defun hyperdrive--entry-directory-p (entry)
  "Return non-nil if ENTRY is a directory."
  (string-suffix-p "/" (hyperdrive-entry-url entry)))

(defun hyperdrive--streamable-p (headers)
  "Return non-nil if response HEADERS indicate that the content is
audio or video which can be streamed with mpv."
  (string-match (rx (or "audio" "video")) (alist-get 'content-type headers)))

(defun hyperdrive--version-match (url)
  "Return non-nil if URL contains a version number."
  (string-match hyperdrive--version-re url))

(defun hyperdrive-message (message &rest args)
  "Call `message' prefixing MESSAGE with \"Hyperdrive:\"."
  (apply #'message (concat "Hyperdrive: " message) args))

(provide 'hyperdrive-lib)
;;; hyperdrive-lib.el ends here
