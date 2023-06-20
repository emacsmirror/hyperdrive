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
(require 'seq)
(require 'url-util)

(require 'compat)
(require 'persist)
(require 'plz)

;;;; Declarations

(declare-function hyperdrive-mode "hyperdrive")
(declare-function hyperdrive-dir-mode "hyperdrive")

;;;; Structs

(cl-defstruct (hyperdrive-entry (:constructor hyperdrive-entry--create)
                                (:copier nil))
  "Represents an entry in a hyperdrive."
  (hyperdrive nil :documentation "The entry's hyperdrive.")
  ;; (url nil :documentation "Canonical URL to entry.")
  ;; Rather than storing just the path and making a function to return
  ;; the name, we store the name as-is because, for one thing, the name
  ;; could theoretically contain a slash, and `file-name-nondirectory'
  ;; would return the wrong value in that case.
  (name nil :documentation "Decoded filename of entry (excluding leading slash).")
  (path nil :documentation "Encoded path (including leading slash).")
  (headers nil :documentation "HTTP headers from request.")
  (modified nil :documentation "Last modified time.")
  (size nil :documentation "Size of file.")
  (version nil :documentation "Hyperdrive version specified in entry's URL.")
  (type nil :documentation "MIME type of the entry.")
  (etc nil :documentation "Alist for extra data about the entry."))

(cl-defstruct (hyperdrive (:constructor hyperdrive-create)
                          (:copier nil))
  "Represents a hyperdrive."
  (public-key nil :documentation "Hyperdrive's public key.")
  (seed nil :documentation "Seed (always and only present for writable hyperdrives).")
  (writablep nil :documentation "Whether the drive is writable.")
  (petname nil :documentation "Petname.")
  ;; TODO: Where to invalidate old domains?
  (domains nil :documentation "List of DNSLink domains which resolve to the drive's public-key.")
  (metadata nil :documentation "Public metadata alist.")
  (latest-version nil :documentation "Latest known version of hyperdrive.")
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

(cl-defun hyperdrive-entry-create (&key hyperdrive path version etc encode)
  "Return hyperdrive entry struct from args.
HYPERDRIVE, VERSION, and ETC are used as-is. Entry NAME is
generated from PATH. When ENCODE is non-`nil', encode PATH."
  (setf path (hyperdrive--format-path path))
  (when encode
    (cl-callf url-hexify-string path (cons ?/ url-unreserved-chars)))
  (hyperdrive-entry--create
   :hyperdrive hyperdrive
   :path path
   :name (url-unhex-string
          (pcase path
           ("/"
            ;; Root directory: use "/" for clarity.
            "/")
           ((pred (string-suffix-p "/"))
            ;; A subdirectory: keep the trailing slash for clarity
            (file-relative-name path (file-name-parent-directory path)))
           (_
            ;; A file: remove directory part.
            (file-name-nondirectory path))))
   :version version
   :etc etc))

;;;; Variables

(defvar-local hyperdrive-current-entry nil
  "Entry for current buffer.")
(put 'hyperdrive-current-entry 'permanent-local t)

(defvar-local hyperdrive-describe-current-hyperdrive nil
  "Hyperdrive for current `hyperdrive-describe-mode' buffer.")
(put 'hyperdrive-describe-current-hyperdrive 'permanent-local t)

(defvar-local hyperdrive-mirror-parent-entry nil
  "Parent entry for `hyperdrive-mirror-mode' buffer.")
(put 'hyperdrive-mirror-parent-entry 'permanent-local t)

(defvar-local hyperdrive-mirror-already-uploaded nil
  "Non-nil if files in `hyperdrive-mirror-mode' buffer have already been uploaded.")

(defvar hyperdrive-timestamp-format-string nil)

(defvar hyperdrive-current-entry)
(defvar hyperdrive-hyper-gateway-port)
(defvar hyperdrive-hyperdrives)
(defvar hyperdrive-default-host-format)
(defvar hyperdrive-honor-auto-mode-alist)
(defvar hyperdrive-version-ranges)

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

REST is passed to `plz', which see.

REST may include the argument `:queue', a `plz-queue' in which to
make the request."
  (declare (indent defun))
  (pcase method
    ((and (or 'get 'head)
          (guard (string-suffix-p "/" url)))
     ;; By default, hypercore-fetch resolves directory URLs to the
     ;; index.html file inside that directory. See
     ;; <https://github.com/RangerMauve/hypercore-fetch#fetchhypernameexamplenoresolve-method-get>
     (setf url (concat url "?noResolve"))))
  (if-let ((queue (prog1 (plist-get rest :queue)
                    (setf rest (map-delete rest :queue)))))
      (plz-run
       (apply #'plz-queue
              queue method (hyperdrive--httpify-url url) rest))
    (apply #'plz method (hyperdrive--httpify-url url) rest)))

(defun hyperdrive--httpify-url (url)
  "Return localhost HTTP URL for HYPER-URL."
  (concat "http://localhost:" (number-to-string hyperdrive-hyper-gateway-port) "/hyper/"
          (substring url (length hyperdrive--hyper-prefix))))

(cl-defun hyperdrive--write (url &key body then else queue)
  "Save BODY (a string) to hyperdrive URL.
THEN and ELSE are passed to `hyperdrive-api', which see."
  (declare (indent defun))
  (hyperdrive-api 'put url
    ;; TODO: Investigate whether we should use 'text body type for text buffers.
    :body-type 'binary
    ;; TODO: plz accepts buffer as a body, we should refactor calls to hyperdrive--write to pass in a buffer instead of a buffer-string.
    :body body :as 'response :then then :else else :queue queue))

(defun hyperdrive-parent (entry)
  "Return parent entry for ENTRY.
If already at top-level directory, return nil."
  (pcase-let (((cl-struct hyperdrive-entry hyperdrive path version) entry))
    (when-let ((parent-path (file-name-parent-directory path)))
      (hyperdrive-entry-create :hyperdrive hyperdrive :path parent-path :version version))))

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
               ;; `hyperdrive-create', but perhaps it would be good to add a function which wraps
               ;; `hyperdrive-create' and returns either an existing hyperdrive or a new one?
               (hyperdrive (pcase host
                             ((rx ".") ; Assume host is a DNSLink domain. See code for <https://github.com/RangerMauve/hyper-sdk#sdkget>.
                              (hyperdrive-create :domains (list host)))
                             (_  ;; Assume host is a public-key
                              (or (gethash host hyperdrive-hyperdrives)
                                  (hyperdrive-create :public-key host)))))
               ;; TODO: Target inside etc is currently unused, consider removing or adding support inside `hyperdrive-handler-default'
               (etc (when target
                      (list (cons 'target target))))
               (version (pcase path
                          ((rx "/$/version/" (let v (1+ num)) (let p (0+ anything)))
                           (setf path p)
                           (string-to-number v)))))
    ;; e.g. for hyper://PUBLIC-KEY/path/to/basename, we do:
    ;; :path "/path/to/basename" :name "basename"
    (hyperdrive-entry-create :hyperdrive hyperdrive :path path :version version :etc etc)))

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

(defun hyperdrive-entry-latest (entry)
  "Return ENTRY at its hyperdrive's latest version, or nil."
  (hyperdrive-entry-at nil entry))

(defun hyperdrive-entry-version-range-start (entry)
  "Return the range start of ENTRY's version, or nil."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path version) entry)
               (entry-key (cons hyperdrive path))
               (ranges (gethash entry-key hyperdrive-version-ranges))
               (range (if version
                          (cl-find-if (pcase-lambda (`(,start . ,(map (:range-end range-end))))
                                        (and (<= start version)
                                             (or (not range-end)
                                                 (>= range-end version))))
                                      ranges)
                        (car (last ranges)))))
    (car range)))

(defun hyperdrive-entry-previous (entry)
  "Return ENTRY at its hyperdrive's previous version, or nil."
  (when-let ((previous-entry (hyperdrive-entry-at (1- (hyperdrive-entry-version-range-start entry)) entry)))
    ;; Entry version is currently its range end, but it should be its version range start.
    (setf (hyperdrive-entry-version previous-entry) (hyperdrive-entry-version-range-start previous-entry))
    previous-entry))

(defun hyperdrive-entry-at (version entry)
  "Return ENTRY at its hyperdrive's VERSION, or nil if not found.
When VERSION is `nil', return latest version of ENTRY."
  ;; Use `hyperdrive-copy-tree', because `copy-tree' doesn't work on
  ;; records/structs, and `copy-hyperdrive-entry' doesn't copy deeply,
  ;; and we need to be able to modify the `etc' alist of the copied
  ;; entry separately.
  (let ((entry (hyperdrive-copy-tree entry t)))
    (setf (hyperdrive-entry-version entry) version)
    (condition-case err
        (hyperdrive-fill entry :then 'sync)
      (plz-http-error
       (pcase (plz-response-status (plz-error-response (caddr err)))
         (404 nil)
         (_ (signal (car err) (cdr err))))))))

(cl-defun hyperdrive-fill
    (entry &key queue then
           (else (lambda (plz-error)
                   ;; FIXME: Use a message instead of a warning for
                   ;; now, because the 404 errors for filenames with
                   ;; spaces are annoying as warnings.
                   (hyperdrive-message (format "hyperdrive-fill: error: %S" plz-error) )
                   ;; (display-warning 'hyperdrive
                   ;;                  (format "hyperdrive-fill: error: %S" plz-error))
                   )))
  "Fill ENTRY's metadata and call THEN.
If THEN is `sync', return the filled entry and ignore ELSE.
Otherwise, make request asynchronously and call THEN with the
filled entry; or if request fails, call ELSE (which is passed to
`hyperdrive-api', which see.  If QUEUE, make the fill request in
the given `plz-queue'"
  (declare (indent defun))
  (pcase then
    ('sync (condition-case err
               (hyperdrive--fill entry
                                 (plz-response-headers
                                  (with-local-quit
                                    (hyperdrive-api 'head (hyperdrive-entry-url entry)
                                      :as 'response
                                      :then 'sync
                                      :noquery t))))
             (plz-http-error
              (pcase (plz-response-status (plz-error-response (caddr err)))
                (404 ;; Entry doesn't exist at this version: update range data.
                 (hyperdrive-update-version-ranges entry (hyperdrive-entry-version entry) :existsp nil)))
              ;; Re-signal error for, e.g. `hyperdrive-entry-at'.
              (signal (car err) (cdr err)))))
    (_ (hyperdrive-api 'head (hyperdrive-entry-url entry)
         :queue queue
         :as 'response
         :then (lambda (response)
                 (funcall then (hyperdrive--fill entry (plz-response-headers response))))
         :else (lambda (&rest args)
                 (hyperdrive-update-version-ranges entry (hyperdrive-entry-version entry) :existsp nil)
                 (apply else args))
         :noquery t))))

(defun hyperdrive--fill (entry headers)
  "Fill ENTRY and its hyperdrive from HEADERS.

The following ENTRY slots are filled:
- type
- modified
- size
- hyperdrive (from persisted value if it exists)

The following ENTRY hyperdrive slots are filled:
- public-key
- domains (merged with current persisted value)"
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive) entry)
               ((map link content-length content-type etag last-modified) headers)
               ;; If URL hostname was a DNSLink domain, entry doesn't yet have a public-key slot.
               (public-key (progn
                             (string-match hyperdrive--public-key-re link)
                             (match-string 1 link)))
               (persisted-hyperdrive (gethash public-key hyperdrive-hyperdrives))
               (domain (car (hyperdrive-domains hyperdrive))))
    (when last-modified
      (setf last-modified (encode-time (parse-time-string last-modified))))
    (setf (hyperdrive-entry-size entry) (when content-length
                                          (ignore-errors
                                            (cl-parse-integer content-length)))
          (hyperdrive-entry-type entry) content-type
          ;; TODO: Rename slot to "mtime" to avoid confusion.
          (hyperdrive-entry-modified entry) last-modified)
    (when domain
      (if persisted-hyperdrive
          ;; The previous call to hyperdrive-entry-url did not retrieve the
          ;; persisted hyperdrive because we had no public-key, only a domain.
          (progn
            (setf (hyperdrive-entry-hyperdrive entry) persisted-hyperdrive)
            (cl-pushnew domain (hyperdrive-domains (hyperdrive-entry-hyperdrive entry)) :test #'equal))
        (setf (hyperdrive-public-key hyperdrive) public-key)))
    (hyperdrive-update-version-ranges entry (string-to-number etag))
    entry))

(defun hyperdrive--latest-version (hyperdrive)
  "Return the latest version number of HYPERDRIVE.
Also sets the corresponding slot in HYPERDRIVE."
  (pcase-let (((cl-struct plz-response (headers (map etag)))
               (with-local-quit
                 (hyperdrive-api
                   'head (hyperdrive-entry-url
                          (hyperdrive-entry-create
                           :hyperdrive hyperdrive :path "/"))
                   :as 'response))))
    (setf (hyperdrive-latest-version hyperdrive) (string-to-number etag))))

;; TODO: Consider using symbol-macrolet to simplify place access.

(cl-defun hyperdrive-update-version-ranges (entry range-start &key (existsp t))
  ;; FIXME: Docstring.
  (unless (hyperdrive--entry-directory-p entry)
    ;; TODO: Revisit whether we really want to not do anything for directories.
    (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
                 (ranges-key (cons hyperdrive path))
                 (entry-ranges (gethash ranges-key hyperdrive-version-ranges))
                 (current-range (map-elt entry-ranges range-start))
                 ((map (:range-end range-end)) current-range))
      (when (or (not range-end)
                (< range-end (hyperdrive-entry-version entry)))
        (setf (plist-get current-range :range-end) (hyperdrive-entry-version entry)
              (map-elt entry-ranges range-start) current-range))
      (setf (plist-get current-range :exists-p) existsp
            (map-elt entry-ranges range-start) current-range
            entry-ranges (cl-sort entry-ranges #'< :key #'car))
      ;; TODO: Destructively decrement range-start (car of cons cell) whenever an entry 404s
      (setf (gethash ranges-key hyperdrive-version-ranges) entry-ranges))))

(defun hyperdrive-fill-metadata (hyperdrive)
  "Fill HYPERDRIVE's public metadata and return it.
Sends a synchronous request to get the latest contents of
HYPERDRIVE's public metadata file."
  (declare (indent defun))
  (pcase-let* ((entry (hyperdrive-entry-create
                       :hyperdrive hyperdrive
                       :path "/.well-known/host-meta.json"
                       ;; NOTE: Don't attempt to fill hyperdrive struct with old metadata
                       :version nil))
               (metadata (with-local-quit
                           (condition-case err
                               (hyperdrive-api 'get (hyperdrive-entry-url entry)
                                 ;; TODO: How to handle invalid JSON? Currently, we get this error:
                                 ;; error in process sentinel: JSON readtable error: 105
                                 :as #'json-read :noquery t)
                             (plz-http-error
                              (pcase (plz-response-status (plz-error-response (caddr err)))
                                (404 nil)
                                (_ (signal (car err) (cdr err)))))))))
    (setf (hyperdrive-metadata hyperdrive) metadata)
    (hyperdrive-persist hyperdrive)
    hyperdrive))

(cl-defun hyperdrive-delete (entry &key then else)
  "Delete ENTRY, then call THEN.
Call ELSE if request fails."
  (declare (indent defun))
  ;; TODO: update-version-ranges here.
  (hyperdrive-api 'delete (hyperdrive-entry-url entry)
    :then then :else else))

(cl-defun hyperdrive-write (entry &key body then else queue)
  "Write BODY to hyperdrive ENTRY's URL."
  (declare (indent defun))
  (hyperdrive--write (hyperdrive-entry-url entry)
    :body body :then then :else else :queue queue))

(cl-defun hyperdrive-entry-description (entry &key (format-path 'path))
  "Return description for ENTRY.
When ENTRY has a non-`nil' VERSION slot, include it. Returned
string looks like:

  FORMAT-PATH [HOST] (version:VERSION)

When FORMAT-PATH is `path', use full path to entry. When
FORMAT-PATH is `name', use only last part of path, as in
`file-name-non-directory'."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive version path name) entry)
               (handle (hyperdrive--format-host hyperdrive
                                                :format hyperdrive-default-host-format
                                                :with-label t)))
    (propertize (concat (format "[%s] " handle)
                        (pcase format-path
                          ('path (url-unhex-string path))
                          ('name name))
                        (when version
                          (format " (version:%s)" version)))
                'help-echo (hyperdrive-entry-url entry))))

(cl-defun hyperdrive--format-entry-url
    (entry &key (host-format '(public-key domain))
           (with-protocol t) (with-help-echo t))
  "Return ENTRY's URL.
Returns URL formatted like:

  hyper://HOST-FORMAT/PATH/TO/FILE

HOST-FORMAT is passed to `hyperdrive--format-host', which see. If
WITH-PROTOCOL, \"hyper://\" is prepended. If WITH-HELP-ECHO,
propertize string with `help-echo' property showing the entry's
full URL. When ENTRY has non-`nil' `version' slot, include
version number in URL.

Note that, if HOST-FORMAT includes values other than `public-key'
and `domain', the resulting URL may not be a valid hyperdrive
URL."
  ;; NOTE: Entries may have only a domain, not a public key yet, so we
  ;; include `domain' in HOST-FORMAT's default value.  The public key
  ;; will be filled in later.
  (pcase-let* (((cl-struct hyperdrive-entry path version)
                entry)
               (protocol (when with-protocol
                           "hyper://"))
               (host (hyperdrive--format-host (hyperdrive-entry-hyperdrive entry)
                                              :format host-format))
               (version-part (and version (format "/$/version/%s" version)))
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

(cl-defun hyperdrive-complete-hyperdrive (&key predicate force-prompt)
  "Return hyperdrive for current entry when it matches PREDICATE.

With FORCE-PROMPT or when current hyperdrive does not match
PREDICATE, return a hyperdrive selected with completion. In this
case, when PREDICATE, only offer hyperdrives matching it."
  (unless predicate
    ;; cl-defun default value doesn't work when nil predicate value is passed in.
    (setf predicate #'always))
  (if-let* (((not force-prompt))
            (hyperdrive-current-entry)
            (current-hyperdrive (hyperdrive-entry-hyperdrive hyperdrive-current-entry))
            ((funcall predicate current-hyperdrive)))
      current-hyperdrive
    (let* ((hyperdrives (cl-remove-if-not predicate (hash-table-values hyperdrive-hyperdrives)))
           (default (when (and hyperdrive-current-entry (funcall predicate current-hyperdrive))
                      (hyperdrive--format-hyperdrive (hyperdrive-entry-hyperdrive hyperdrive-current-entry))))
           (prompt (if default
                       (format "Hyperdrive (default %s): " default)
                     "Hyperdrive: "))
           (candidates (mapcar (lambda (hyperdrive)
                                 (cons (hyperdrive--format-hyperdrive hyperdrive) hyperdrive))
                               hyperdrives))
           (completion-styles (cons 'substring completion-styles))
           (selected (completing-read prompt candidates nil 'require-match nil nil default)))
      (or (alist-get selected candidates nil nil #'equal)
          (user-error "No such hyperdrive.  Use `hyperdrive-new' to create a new one")))))

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

(cl-defun hyperdrive-read-entry (&key predicate default-path (allow-version-p t) force-prompt)
  "Return new hyperdrive entry with path and hyperdrive read from user.
Prompts user for a hyperdrive and signals an error if no such
hyperdrive is known.  If DEFAULT-PATH, offer it as the default entry path.

PREDICATE and FORCE-PROMPT are passed to
`hyperdrive-complete-hyperdrive', which see.

When ALLOW-VERSION-P is nil, returned entry's version slot will be
nil.  When ALLOW-VERSION-P is non-nil, FORCE-PROMPT is nil and
entry is for the same hyperdrive as `hyperdrive-current-entry',
returned entry uses its version slot.  Otherwise, prompt for a
version number."
  (cl-callf hyperdrive--format-path default-path)
  (let* ((hyperdrive (hyperdrive-complete-hyperdrive :predicate predicate
                                                     :force-prompt force-prompt))
         (current-version (when (and allow-version-p
                                     hyperdrive-current-entry
                                     (equal hyperdrive (hyperdrive-entry-hyperdrive hyperdrive-current-entry)))
                            (hyperdrive-entry-version hyperdrive-current-entry)))
         (version (when allow-version-p
                    (if force-prompt
                        (hyperdrive-read-version :hyperdrive hyperdrive :initial-input-number current-version)
                      current-version)))
         (path (hyperdrive-read-path :hyperdrive hyperdrive :version version :default default-path)))
    (hyperdrive-entry-create :hyperdrive hyperdrive :path path :version version :encode t)))

(defvar hyperdrive--version-history nil
  "Minibuffer history of `hyperdrive-read-version'.")

(cl-defun hyperdrive-read-version (&key hyperdrive prompt initial-input-number)
  "Return version number.
Blank input returns nil.

HYPERDRIVE is used to fill in PROMPT format %s sequence.
INITIAL-INPUT is converted to a string and passed to
`read-string', which see."
  (let* ((prompt (or prompt "Version number in «%s» (leave blank for latest version)"))
         ;; Don't use read-number since it cannot return nil.
         (version (read-string
                   (format-prompt prompt nil (hyperdrive--format-hyperdrive hyperdrive))
                   (when initial-input-number (number-to-string initial-input-number))
                   'hyperdrive-read-version)))
    (unless (string-blank-p version)
      (string-to-number version))))

(defvar hyperdrive--path-history nil
  "Minibuffer history of `hyperdrive-read-path'.")

(cl-defun hyperdrive-read-path (&key hyperdrive version prompt default)
  "Return path read from user.
HYPERDRIVE and VERSION are used to fill in the prompt's format %s
sequence.  PROMPT is passed to `format-prompt', which see.  DEFAULT
is passed to `read-string' as both its INITIAL-INPUT and
DEFAULT-VALUE arguments."
  (let ((prompt (or prompt
                    (if version
                        "Path in «%s» (version:%s)"
                      "Path in «%s»"))))
    ;; TODO: Provide a `find-file'-like auto-completing UI
    (read-string (format-prompt prompt default
                                (hyperdrive--format-hyperdrive hyperdrive) version)
                 default 'hyperdrive--path-history default)))

(defvar hyperdrive--url-history nil
  "Minibuffer history of `hyperdrive-read-url'.")

(cl-defun hyperdrive-read-url (&key (prompt "Hyperdrive URL"))
  "Return URL trimmed of whitespace.
Prompts with PROMPT. Defaults to current entry if it exists."
  (let ((default (when hyperdrive-current-entry
                   (hyperdrive-entry-url hyperdrive-current-entry))))
    (string-trim (read-string (format-prompt prompt default) nil 'hyperdrive--url-history default))))

(defvar hyperdrive--name-history nil
  "Minibuffer history of `hyperdrive-read-name'.")

(cl-defun hyperdrive-read-name (&key prompt initial-input default)
  "Wrapper for `read-string' with common history.
Prompts with PROMPT."
  (read-string (format-prompt prompt default) initial-input 'hyperdrive--name-history default))

(cl-defun hyperdrive-put-metadata (hyperdrive &key then)
  "Put HYPERDRIVE's metadata into the appropriate file, then call THEN."
  (declare (indent defun))
  (let ((entry (hyperdrive-entry-create :hyperdrive hyperdrive
                                        :path "/.well-known/host-meta.json")))
    (hyperdrive-write entry :body (json-encode (hyperdrive-metadata hyperdrive))
      :then then)
    hyperdrive))

(defun hyperdrive-persist (hyperdrive)
  "Persist HYPERDRIVE in `hyperdrive-hyperdrives'."
  (puthash (hyperdrive-public-key hyperdrive) hyperdrive hyperdrive-hyperdrives)
  (persist-save 'hyperdrive-hyperdrives))

(defun hyperdrive-seed-url (seed)
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

;;;###autoload
(defun hyperdrive-by-slot (slot value)
  "Return persisted hyperdrive struct whose SLOT matches VALUE.
Otherwise, return `nil'.  SLOT may be one of

- seed
- petname
- public-key"
  (let ((accessor-function (pcase-exhaustive slot
                             ('seed #'hyperdrive-seed)
                             ('petname #'hyperdrive-petname)
                             ('public-key #'hyperdrive-public-key))))
    (catch 'get-first-hash
      (maphash (lambda (_key val)
                 (when (equal (funcall accessor-function val) value)
                   (throw 'get-first-hash val)))
               hyperdrive-hyperdrives)
      nil)))

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
    (when (hyperdrive--entry-directory-p entry)
      (hyperdrive-dir-mode))
    (hyperdrive-mode)
    (setq-local hyperdrive-current-entry entry)
    (current-buffer)))

(defun hyperdrive--entry-buffer-name (entry)
  "Return buffer name for ENTRY."
  (hyperdrive-entry-description entry :format-path 'name))

(defun hyperdrive--entry-directory-p (entry)
  "Return non-nil if ENTRY is a directory."
  (string-suffix-p "/" (hyperdrive-entry-url entry)))

(defun hyperdrive-message (message &rest args)
  "Call `message' with MESSAGE and ARGS, prefixing MESSAGE with \"Hyperdrive:\"."
  (apply #'message (concat "Hyperdrive: " message) args))

(defun hyperdrive-copy-tree (tree &optional vecp)
  "Like `copy-tree', but with VECP, works for records too."
  ;; TODO: Now that the new copy-tree behavior has been merged into Emacs,
  ;; remove this function once compat.el supports the new behavior.
  (if (consp tree)
      (let (result)
	(while (consp tree)
	  (let ((newcar (car tree)))
	    (if (or (consp (car tree)) (and vecp (vectorp (car tree))))
		(setq newcar (copy-tree (car tree) vecp)))
	    (push newcar result))
	  (setq tree (cdr tree)))
	(nconc (nreverse result)
               (if (and vecp (or (vectorp tree) (recordp tree))) (copy-tree tree vecp) tree)))
    (if (and vecp (or (vectorp tree) (recordp tree)))
	(let ((i (length (setq tree (copy-sequence tree)))))
	  (while (>= (setq i (1- i)) 0)
	    (aset tree i (copy-tree (aref tree i) vecp)))
	  tree)
      tree)))

(cl-defun hyperdrive--format-path (path &key directoryp)
  "Return PATH with a leading slash if it lacks one.
When DIRECTORYP, also add a trailing slash to PATH if it lacks one.
When PATH is nil or blank, return \"/\"."
  (if (or (not path) (string-blank-p path))
      "/"
    (expand-file-name (if directoryp
                          (file-name-as-directory path)
                        path)
                      "/")))

(provide 'hyperdrive-lib)
;;; hyperdrive-lib.el ends here
