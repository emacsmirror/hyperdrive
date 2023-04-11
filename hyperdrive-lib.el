;;; hyperdrive-lib.el --- Library functions and structures  -*- lexical-binding: t; -*-

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
(declare-function hyperdrive-open "hyperdrive")

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
  (version nil :documentation "Version of hyperdrive for this entry.")
  (type nil :documentation "MIME type of the entry.")
  (etc nil :documentation "Alist for extra data about the entry."))

(cl-defstruct hyperdrive
  "Represents a hyperdrive."
  (public-key nil :documentation "Hyperdrive's public key.")
  (seed nil :documentation "Seed (always and only present for writable hyperdrives).")
  (writablep nil :documentation "Whether the drive is writable.")
  (petname nil :documentation "Petname.")
  ;; TODO: Where to invalidate old domains?
  (domains nil :documentation "List of DNSLink domains which resolve to the drive's public-key.")
  (metadata nil :documentation "Public metadata alist.")
  (etc nil :documentation "Alist of extra data."))

(defun hyperdrive-url (hyperdrive)
  "Return a \"hyper://\"-prefixed URL from a HYPERDRIVE struct.
URL does not have a trailing slash, i.e., \"hyper://PUBLIC-KEY\".

If HYPERDRIVE's public-key slot is empty, use first domain in
domains slot."
  ;; TODO: Add option to prefer domain over public-key
  (pcase-let* (((cl-struct hyperdrive public-key domains) hyperdrive)
               ;; TODO: Fallback to secondary domains?
               (host (or public-key (car domains))))
    (concat "hyper://" host)))

(defun hyperdrive-entry-url (entry)
  "Return ENTRY's canonical URL.
Returns URL with hyperdrive's full public key."
  (hyperdrive--format-entry-url entry :with-protocol t))

;;;; Variables

(defvar hyperdrive-timestamp-format-string nil)

(defvar hyperdrive-current-entry)
(defvar hyperdrive-hyper-gateway-port)
(defvar hyperdrive-hyperdrives)
(defvar hyperdrive-default-host-format)
(defvar hyperdrive-honor-auto-mode-alist)

(eval-and-compile
  (defconst hyperdrive--hyper-prefix "hyper://"
    "Hyperdrive URL prefix."))

(defconst hyperdrive--public-key-re
  (rx (eval hyperdrive--hyper-prefix) (group (= 52 alphanumeric)))
  "Regexp to match \"hyper://\" + public key.

Capture group matches public key.")

(defconst hyperdrive--version-re
  (rx (eval hyperdrive--hyper-prefix)
      (one-or-more alnum)
      (group "+" (one-or-more num)))
  "Regexp to match \"hyper://\" + public key or seed + version number.

Capture group matches version number.")

;;;; API

;; These functions take a URL argument, not a hyperdrive-entry struct.

(cl-defun hyperdrive-api (method url &rest rest)
  "Make hyperdrive API request by METHOD to URL.
Calls `hyperdrive--httpify-url' to convert HYPER-URL starting
with `hyperdrive--hyper-prefix' to a URL starting with
\"http://localhost:4973/hyper/\" (assuming that
`hyper-gateway-port' is \"4973\").

REST is passed to `plz', which see."
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
    ;; TODO: plz accepts buffer as a body, we should refactor calls to hyperdrive--write to pass in a buffer instead of a buffer-string.
    :body body
    :then then :else else))

(defun hyperdrive-entry-exists-p (entry)
  "Return non-nil if ENTRY exists.
Makes a synchronous HEAD request for ENTRY."
  (with-local-quit
    (condition-case err
        (hyperdrive-api 'head (hyperdrive-entry-url entry) :noquery t)
      (plz-http-error
       (if (= 404 (plz-response-status (plz-error-response (cdr err))))
           ;; 404 means entry doesn't exist: return nil.
           nil
         ;; Other error: re-signal.
         (signal (car err) (cdr err)))))))

(defun hyperdrive-parent (entry)
  "Return parent entry for ENTRY.
If already at top-level directory, return nil."
  ;; TODO: Handle versioning.
  (pcase (hyperdrive-entry-path entry)
    ("/"  ;; Already at root: return nil.
     nil)
    (_  ;; Not at root: return parent entry.
     (make-hyperdrive-entry
      :hyperdrive (hyperdrive-entry-hyperdrive entry)
      :path (file-name-directory (directory-file-name (hyperdrive-entry-path entry)))))))

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
Set entry's hyperdrive slot to persisted hyperdrive if it exists.

If URL host is a DNSLink domain, returned entry will have an
empty public-key slot."
  (pcase-let* (((cl-struct url host (filename path) target)
                (url-generic-parse-url url))
               ;; TODO: For now, no other function besides `hyperdrive-url-entry' calls
               ;; `make-hyperdrive', but perhaps it would be good to add a function which wraps
               ;; `make-hyperdrive' and returns either an existing hyperdrive or a new one?
               (hyperdrive (pcase host
                             ((rx ".") ; Assume host is a DNSLink domain. See code for <https://github.com/RangerMauve/hyper-sdk#sdkget>.
                              (make-hyperdrive :domains (list host)))
                             (_  ;; Assume host is a public-key
                              (or (gethash host hyperdrive-hyperdrives)
                                  (make-hyperdrive :public-key host)))))
               (etc (when target
                      (list (cons 'target target))))
               (version (when (string-match (rx "/$/version/" (group (1+ digit))
                                                (group (optional "/" (1+ anything))))
                                            path)
                          (prog1 (string-to-number (match-string 1 path))
                            (setf path (match-string 2 path))
                            (setf (alist-get 'with-version etc) t)))))
    ;; e.g. for hyper://PUBLIC-KEY/path/to/basename, we do:
    ;; :path "/path/to/basename" :name "basename"
    (make-hyperdrive-entry
     :hyperdrive hyperdrive
     :path (if (string-empty-p path) "/" path)
     :name (pcase path
             ((or "" "/")
              ;; Root directory: use "/" for clarity.
              "/")
             ((pred (string-suffix-p "/"))
              ;; A subdirectory: keep the trailing slash for clarity
              ;; (I'm sure this makes sense to someone...).
              (file-name-as-directory
               (file-name-nondirectory (directory-file-name (url-unhex-string path)))))
             (_
              ;; A file: remove directory part.
              (file-name-nondirectory (url-unhex-string path))))
     ;; If version is not in the URL, the slot will be nil, but it
     ;; will be filled elsewhere.
     :version version
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
  "Fill ENTRY and its hyperdrive from HEADERS.

The following ENTRY slots are filled:
- name
- type
- version
- modified

The following ENTRY hyperdrive slots are filled:
- public-key
- domains (merged with current persisted value)"
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive name path) entry)
               ((map link content-length content-type etag last-modified) headers)
               ;; If URL hostname was a DNSLink domain, entry doesn't yet have a public-key slot.
               (public-key (progn
                             (string-match hyperdrive--public-key-re link)
                             (match-string 1 link)))
               (persisted-hyperdrive (gethash public-key hyperdrive-hyperdrives))
               (domain (car (hyperdrive-domains hyperdrive))))
    (unless name
      (setf (hyperdrive-entry-name entry) (string-trim path "/")))
    (when last-modified
      (setf last-modified (encode-time (parse-time-string last-modified))))
    (setf (hyperdrive-entry-size entry) (when content-length
                                          (ignore-errors
                                            (cl-parse-integer content-length)))
          (hyperdrive-entry-type entry) content-type
          ;; FIXME: When
          ;; <https://github.com/RangerMauve/hypercore-fetch/issues/65>
          ;; is done, remove this `1+`.
          (hyperdrive-entry-version entry) (1+ (string-to-number etag))
          (hyperdrive-entry-modified entry) last-modified)
    (when domain
      (if persisted-hyperdrive
          ;; The previous call to hyperdrive-entry-url did not retrieve the
          ;; persisted hyperdrive because we had no public-key, only a domain.
          (progn
            (setf (hyperdrive-entry-hyperdrive entry) persisted-hyperdrive)
            (cl-pushnew domain (hyperdrive-domains (hyperdrive-entry-hyperdrive entry)) :test #'equal))
        (setf (hyperdrive-public-key hyperdrive) public-key)))
    entry))

;; TODO: Call `hyperdrive-fill-public-metadata' in other appropriate places.
(defun hyperdrive-fill-public-metadata (hyperdrive)
  "Fill HYPERDRIVE's public metadata and return it.
Looks in filenames from `hyperdrive-public-metadata-filenames'.
When HYPERDRIVE has a public metadata file, another request is
made synchronously for its contents."
  (declare (indent defun))
  (pcase-let* ((entry (make-hyperdrive-entry :hyperdrive hyperdrive
                                             :path "/.well-known/host-meta.json"))
               (metadata (with-local-quit
                           (hyperdrive-api 'get (hyperdrive-entry-url entry)
                             :as #'json-read :else #'ignore :noquery t))))
    ;; NOTE: RFC6415 specifies that what we use as a "nickname" have
    ;; the key "name" in the JSON object.
    (when metadata
      (setf (hyperdrive-metadata hyperdrive) metadata)
      (hyperdrive-persist hyperdrive))
    hyperdrive))

;; TODO: This.
;; (defun hyperdrive-set-public-metadata (hyperdrive)
;;   )

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

(cl-defun hyperdrive-entry-description (entry &key (with-version t))
  "Return description for ENTRY.
If WITH-VERSION, include it.  Returned string looks like:

  PATH [HOST] (version:VERSION)"
  ;; TODO: When we implement parsing of versions in URLs, update this
  ;; function to automatically include the version when the URL does,
  ;; and not otherwise.
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive version path) entry)
               (handle (hyperdrive--format-host hyperdrive
                                                :format hyperdrive-default-host-format
                                                :with-label t)))
    (propertize (concat (format "[%s] " handle)
                        (url-unhex-string path)
                        (when with-version
                          (format " (version:%s)" version)))
                'help-echo (hyperdrive-entry-url entry))))

(cl-defun hyperdrive--format-entry-url
    (entry &key (host-format '(public-key domain))
           (with-protocol t) (with-help-echo t))
  "Return ENTRY's URL.
Returns URL formatted like:

  hyper://HOST-FORMAT/PATH/TO/FILE

HOST-FORMAT is passed to `hyperdrive--format-host', which see.
If WITH-PROTOCOL, \"hyper://\" is prepended.  If WITH-HELP-ECHO,
propertize string with `help-echo' property showing the entry's
full URL.  If ENTRY's `etc' map has WITH-VERSION set, include
version number in URL.

Note that, if HOST-FORMAT includes values other than `public-key'
and `domain', the resulting URL may not be a valid hyperdrive
URL."
  ;; NOTE: Entries may have only a domain, not a public key yet, so we
  ;; include `domain' in HOST-FORMAT's default value.  The public key
  ;; will be filled in later.
  (pcase-let* (((cl-struct hyperdrive-entry path version
                           (etc (map with-version)))
                entry)
               (protocol (when with-protocol
                           "hyper://"))
               (host (hyperdrive--format-host (hyperdrive-entry-hyperdrive entry)
                                              :format host-format))
               (version-part (and with-version (format "/$/version/%s" version)))
               (url (concat protocol host version-part path)))
    (if with-help-echo
        (propertize url
                    'help-echo (hyperdrive--format-entry-url
                                entry :with-protocol t :host-format '(public-key domain)
                                :with-help-echo nil))
      url)))

(cl-defun hyperdrive--format-host (hyperdrive &key format with-label)
  "Return HYPERDRIVE's formatted hostname, or nil.
FORMAT should be a list of symbols; see
`hyperdrive-default-host-format' for choices.  If the specified
FORMAT is not available, returns nil.  If WITH-LABEL, prepend a
label for the kind of format used (e.g. \"petname:\")."
  (pcase-let* (((cl-struct hyperdrive petname public-key domains seed
                           (metadata (map name)))
                hyperdrive))
    (cl-loop for f in format
             when (pcase f
                    ((and 'petname (guard petname))
                     (concat (when with-label
                               "petname:")
                             (propertize petname 'face 'hyperdrive-petname)))
                    ((and 'nickname (guard name))
                     (concat (when with-label
                               "nickname:")
                             (propertize name
                                         'face 'hyperdrive-nickname)))
                    ((and 'domain (guard (car domains)))
                     ;; TODO: Handle the unlikely case that a drive has multiple domains.
                     (concat (when with-label
                               "domain:")
                             (propertize (car domains) 'face 'hyperdrive-domain)))
                    ((and 'seed (guard seed))
                     (concat (when with-label
                               "seed:")
                             (propertize seed 'face 'hyperdrive-seed)))
                    ((and 'short-key (guard public-key))
                     ;; TODO: Consider adding a help-echo with the full key.
                     (concat (when with-label
                               "public-key:")
                             (propertize (concat (substring public-key 0 6) "…")
                                         'face 'hyperdrive-public-key)))
                    ((and 'public-key (guard public-key))
                     (concat (when with-label
                               "public-key:")
                             (propertize public-key 'face 'hyperdrive-public-key))))
             return it)))

;;;; Reading from the user

(cl-defun hyperdrive-complete-hyperdrive (&key predicate (prompt "Hyperdrive: "))
  "Return a hyperdrive selected with completion.
If PREDICATE, only offer hyperdrives matching it.  Prompt with
PROMPT."
  (let* ((hyperdrives (cl-remove-if-not predicate (hash-table-values hyperdrive-hyperdrives)))
         (default (when hyperdrive-current-entry
                    (hyperdrive--format-hyperdrive (hyperdrive-entry-hyperdrive hyperdrive-current-entry))))
         (candidates (mapcar (lambda (hyperdrive)
                               (cons (hyperdrive--format-hyperdrive hyperdrive) hyperdrive))
                             hyperdrives))
         (completion-styles (cons 'substring completion-styles))
         (selected (completing-read prompt candidates nil 'require-match nil nil default)))
    (or (alist-get selected candidates nil nil #'equal)
        (user-error "No such hyperdrive.  Use `hyperdrive-new' to create a new one"))))

(cl-defun hyperdrive--format-hyperdrive (hyperdrive)
  "Return HYPERDRIVE formatted for completion."
  (let ((petname (hyperdrive--format-host hyperdrive :format '(petname) :with-label t))
        (nickname (hyperdrive--format-host hyperdrive :format '(nickname) :with-label t))
        (domain (hyperdrive--format-host hyperdrive :format '(domain) :with-label t))
        (seed (hyperdrive--format-host hyperdrive :format '(seed) :with-label t))
        (short-key (hyperdrive--format-host hyperdrive :format '(short-key) :with-label t)))
    (string-trim
     (cl-loop for value in (list petname nickname domain seed short-key)
              when value
              concat (concat value "  ")))))

(cl-defun hyperdrive-read-entry (&key predicate)
  "Return new hyperdrive entry with path and hyperdrive read from user.
Prompts user for a hyperdrive and signals an error if no
such hyperdrive is known.
If PREDICATE, only offer hyperdrives matching it."
  (let* ((hyperdrive (hyperdrive-complete-hyperdrive :predicate predicate))
         (default "/")
         (prompt (format "File path (default %S): " default))
         (path (url-hexify-string (read-string prompt default nil default)
                                  (cons ?/ url-unreserved-chars))))
    (make-hyperdrive-entry :hyperdrive hyperdrive
                           :name (file-name-nondirectory path)
                           :path (if (string-prefix-p "/" path)
                                     path
                                   (concat "/" path)))))

(defun hyperdrive-set-petname (petname hyperdrive)
  "Set HYPERDRIVE's PETNAME.
Entering an empty or blank string unsets the HYPERDRIVE's
petname."
  (interactive
   (let* ((hyperdrive (hyperdrive-complete-hyperdrive))
          (petname (read-string
                    (format "Petname (%s): "
                            (hyperdrive--format-host hyperdrive :format '(short-key))))))
     (list petname hyperdrive)))
  (if (string-blank-p petname)
      (setf petname nil)
    (when-let ((other-hyperdrive
                (cl-find petname (hash-table-values hyperdrive-hyperdrives)
                         :key #'hyperdrive-petname :test #'equal)))
      (user-error "Petname %S already assigned to hyperdrive: %s"
                  petname (hyperdrive--format-hyperdrive other-hyperdrive))))
  (setf (hyperdrive-petname hyperdrive) petname)
  ;; TODO: Consider refreshing buffer names, directory headers, etc.
  hyperdrive)

(defun hyperdrive-set-nickname (nickname hyperdrive)
  "Set HYPERDRIVE's NICKNAME.
Entering an empty or blank string unsets the HYPERDRIVE's
nickname."
  (interactive
   (let* ((hyperdrive (hyperdrive-complete-hyperdrive :predicate #'hyperdrive-writablep))
          (nickname (read-string
                     (format "New nickname for hyperdrive (%s): "
                             (hyperdrive--format-host hyperdrive :format '(short-key)))
                     (alist-get 'name (hyperdrive-metadata hyperdrive)))))
     (list nickname hyperdrive)))
  (when (string-blank-p nickname)
    (setf nickname nil))
  (hyperdrive-fill-public-metadata hyperdrive)
  (setf (alist-get 'name (hyperdrive-metadata hyperdrive)) nickname)
  (hyperdrive-put-metadata hyperdrive
    :then (lambda (&rest _)
            (hyperdrive-message "Set nickname: %s"
                                (hyperdrive--format-host hyperdrive :format '(nickname)))))
  ;; TODO: Consider refreshing buffer names, directory headers, etc.
  hyperdrive)

(cl-defun hyperdrive-put-metadata (hyperdrive &key then)
  "Put HYPERDRIVE's metadata into the appropriate file."
  (declare (indent defun))
  (let ((entry (make-hyperdrive-entry :hyperdrive hyperdrive
                                      :path "/.well-known/host-meta.json")))
    (hyperdrive-write entry :body (json-encode (hyperdrive-metadata hyperdrive))
      :then then)
    hyperdrive))

;;;###autoload
(defun hyperdrive-new (seed)
  "Open new hyperdrive for SEED."
  (interactive (list (read-string "New hyperdrive seed: ")))
  (let* ((response (with-local-quit
                     (hyperdrive-api 'post (concat "hyper://localhost/?key=" (url-hexify-string seed)))))
         (url (progn
                ;; NOTE: Working around issue in plz whereby the
                ;; stderr process sentinel sometimes leaves "stderr
                ;; finished" garbage in the response body in older
                ;; Emacs versions.  See: <https://github.com/alphapapa/plz.el/issues/23>.
                (string-match (rx bos (group "hyper://" (1+ nonl))) response)
                (match-string 1 response)))
         (hyperdrive (hyperdrive-entry-hyperdrive (hyperdrive-url-entry url))))
    (setf (hyperdrive-seed hyperdrive) seed
          (hyperdrive-writablep hyperdrive) t)
    ;; Persist the hyperdrive before setting the pet name in case a
    ;; conflict prevents this function from returning naturally.
    (hyperdrive-persist hyperdrive)
    (condition-case nil
        (hyperdrive-set-petname seed hyperdrive)
      (user-error (hyperdrive-set-petname
                   (read-string
                    (format "%S already assigned as petname to hyperdrive: %s.  Enter new petname: "
                            seed (hyperdrive--format-hyperdrive
                                  (cl-find seed (hash-table-values hyperdrive-hyperdrives)
                                           :key #'hyperdrive-petname :test #'equal))))
                   hyperdrive)))
    (hyperdrive-persist hyperdrive)
    (hyperdrive-open (hyperdrive-url-entry url))))

(defun hyperdrive-persist (hyperdrive)
  "Persist HYPERDRIVE in `hyperdrive-hyperdrives'."
  (puthash (hyperdrive-public-key hyperdrive) hyperdrive hyperdrive-hyperdrives)
  (persist-save 'hyperdrive-hyperdrives))

(defun hyperdrive--seed-url (seed)
  "Return URL to hyperdrive known as SEED, or nil if it doesn't exist.
That is, if the SEED has been used to create a local
hyperdrive."
  (condition-case err
      (pcase (with-local-quit
               (hyperdrive-api 'get (concat "hyper://localhost/?key=" (url-hexify-string seed))
                               :as 'response :noquery t))
        ((and (pred plz-response-p)
              response
              (guard (= 200 (plz-response-status response))))
         (plz-response-body response)))
    (plz-http-error (if (= 400 (plz-response-status (plz-error-response (caddr err))))
                        nil
                      (signal 'plz-http-error err)))))

;;;; Misc.

(defun hyperdrive--get-buffer-create (entry)
  "Return buffer for ENTRY.
In the buffer, `hyperdrive-mode' is activated and
`hyperdrive-current-entry' is set.

This function helps prevent duplicate `hyperdrive-mode' buffers
by ensuring that buffer names always use the namespace seed
corresponding to URL if possible.

In other words, this avoids the situation where a buffer called
\"foo:/\" and another called \"hyper://<public key for foo>/\"
both point to the same content."
  (with-current-buffer (get-buffer-create (hyperdrive--entry-buffer-name entry))
    (when hyperdrive-honor-auto-mode-alist
      ;; Inspired by https://emacs.stackexchange.com/a/2555/39549
      (let ((buffer-file-name (hyperdrive-entry-name entry)))
        (set-auto-mode)))
    (hyperdrive-mode)
    (setq-local hyperdrive-current-entry entry)
    (current-buffer)))

(defun hyperdrive--entry-buffer-name (entry)
  "Return buffer name for ENTRY."
  (format "[%s] %s%s"
          (hyperdrive--format-host (hyperdrive-entry-hyperdrive entry)
                                   :format hyperdrive-default-host-format
                                   :with-label t)
          (hyperdrive-entry-name entry)
          (if (alist-get 'with-version (hyperdrive-entry-etc entry))
              (format " (version:%s)" (hyperdrive-entry-version entry))
            "")))

(defun hyperdrive--entry-directory-p (entry)
  "Return non-nil if ENTRY is a directory."
  (string-suffix-p "/" (hyperdrive-entry-url entry)))

(defun hyperdrive-message (message &rest args)
  "Call `message' with MESSAGE and ARGS, prefixing MESSAGE with \"Hyperdrive:\"."
  (apply #'message (concat "Hyperdrive: " message) args))

(provide 'hyperdrive-lib)
;;; hyperdrive-lib.el ends here
