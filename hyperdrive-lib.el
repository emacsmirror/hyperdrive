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

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'pcase)
(require 'seq)
(require 'url-util)
(require 'gv)

(require 'compat)
(require 'persist)
(require 'plz)

(require 'hyperdrive-vars)

;;;; Declarations

(declare-function hyperdrive-mode "hyperdrive")
(declare-function hyperdrive-dir-mode "hyperdrive-dir")

;;;; Errors

(define-error 'hyperdrive-error "hyperdrive error")

(defun hyperdrive-error (&rest args)
  "Like `error', but signals `hyperdrive-error'.
Passes ARGS to `format-message'."
  (signal 'hyperdrive-error (list (apply #'format-message args))))

;;;; Structs

(cl-defstruct (hyperdrive-entry (:constructor hyperdrive-entry--create)
                                (:copier nil))
  "Represents an entry in a hyperdrive."
  (hyperdrive nil :documentation "The entry's hyperdrive.")
  ;; Rather than storing just the path and making a function to return
  ;; the name, we store the name as-is because, for one thing, the name
  ;; could theoretically contain a slash, and `file-name-nondirectory'
  ;; would return the wrong value in that case.
  (name nil :documentation "Decoded filename of entry (excluding leading slash).")
  (path nil :documentation "Encoded path (including leading slash).")
  (headers nil :documentation "HTTP headers from request.")
  (mtime nil :documentation "Last modified time.")
  (size nil :documentation "Size of file.")
  (version nil :documentation "Hyperdrive version specified in entry's URL.")
  (type nil :documentation "MIME type of the entry.")
  (etc nil :documentation "Alist for extra data about the entry."))

(cl-defstruct (hyperdrive (:constructor hyperdrive-create)
                          (:copier nil))
  "Represents a hyperdrive."
  (public-key nil :documentation "Hyperdrive's public key.")
  (seed nil :documentation "Seed (always and only present for writable hyperdrives).")
  (writablep 'unknown :documentation "Whether the drive is writable.")
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
HYPERDRIVE, VERSION, and ETC are used as-is.  Entry NAME is
generated from PATH.  When ENCODE is non-nil, encode PATH."
  (setf path (hyperdrive--format-path path))
  (when encode
    (cl-callf url-hexify-string path (cons ?/ url-unreserved-chars)))
  (hyperdrive-entry--create
   :hyperdrive hyperdrive
   :path path
   ;; TODO: Is it necessary to store the name alongside the path?
   ;;       Instead, only store path and generate name on the fly.
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

(cl-defun hyperdrive-sort-entries (entries &key (direction hyperdrive-directory-sort))
  "Return ENTRIES sorted by DIRECTION.
See `hyperdrive-directory-sort' for the type of DIRECTION."
  (pcase-let* ((`(,column . ,direction) direction)
               ((map (:accessor accessor) (direction sort-function))
                (alist-get column hyperdrive-dir-sort-fields)))
    (cl-sort entries (lambda (a b)
                       (cond ((and a b) (funcall sort-function a b))
                             ;; When an entry lacks appropriate metadata
                             ;; for sorting by DIRECTION, put it at the end.
                             (a t)))
             :key accessor)))

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
  ;; TODO: Document that the request/queue is returned.
  ;; TODO: Should we create a wrapper for `hyperdrive-api' which calls
  ;;  `hyperdrive--fill-latest-version' for requests to
  ;;  directories/requests which modify the drive (and therefore
  ;;  always return the latest version number). If we did this, we
  ;;  could remove redundant calls to
  ;;  `hyperdrive--fill-latest-version' everywhere else.
  (declare (indent defun))
  (pcase method
    ((and (or 'get 'head)
          (guard (string-suffix-p "/" url)))
     ;; By default, hypercore-fetch resolves directory URLs to the
     ;; index.html file inside that directory. See
     ;; <https://github.com/RangerMauve/hypercore-fetch#fetchhypernameexamplenoresolve-method-get>
     (setf url (concat url "?noResolve"))))
  (pcase-let* ((else (pcase (plist-get rest :then)
                       ((or `nil 'sync)
                        ;; In keeping with `plz', ignore ELSE for sync requests.
                        nil)
                       (_ (plist-get rest :else))))
               ;; We wrap the provided ELSE in our own lambda that
               ;; checks for common errors.
               (else* (apply-partially #'hyperdrive-api-default-else else)))
    (plist-put rest :else else*)
    (condition-case err
        ;; The `condition-case' is only intended for synchronous
        ;; requests.  Async requests should never signal a `plz-error'
        ;; directly from `plz' or `plz-run'.
        (if-let ((queue (prog1 (plist-get rest :queue)
                          (setf rest (map-delete rest :queue)))))
            (plz-run
             (apply #'plz-queue
                    queue method (hyperdrive--httpify-url url) rest))
          (apply #'plz method (hyperdrive--httpify-url url) rest))
      (plz-error
       ;; We pass only the `plz-error' struct to the ELSE* function.
       (funcall else* (caddr err))))))

(defun hyperdrive-api-default-else (else plz-err)
  "Handle common errors, overriding ELSE.
Checks for common errors; if none are found, calls ELSE with
PLZ-ERR, if ELSE is non-nil; otherwise re-signals PLZ-ERR.
PLZ-ERR should be a `plz-error' struct."
  (pcase plz-err
    ((app plz-error-curl-error `(7 . ,_message))
     ;; Curl error 7 is "Failed to connect to host."
     (hyperdrive-user-error (substitute-command-keys
                             "Gateway not running.  Use \\[hyperdrive-start] to start it")))
    ((app plz-error-response (cl-struct plz-response (status (or 403 405)) body))
     ;; 403 Forbidden or 405 Method Not Allowed: Display message from hyper-gateway.
     (hyperdrive-error "%s" body))
    ((guard else)
     (funcall else plz-err))
    (_
     (signal 'plz-error (list "plz error" plz-err)))))

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

;; For Emacsen <29.1.
(declare-function textsec-suspicious-p "ext:textsec-check")
(defun hyperdrive-url-entry (url)
  "Return entry for URL.
Set entry's hyperdrive slot to persisted hyperdrive if it exists.

If URL host is a DNSLink domain, returned entry will have an
empty public-key slot.

If URL does not begin with \"hyper://\" prefix, it will be added
before making the entry struct."
  (unless (string-prefix-p "hyper://" url)
    (setf url (concat "hyper://" url)))
  (pcase-let* (((cl-struct url host (filename path) target)
                (url-generic-parse-url url))
               ;; TODO: For now, no other function besides `hyperdrive-url-entry' calls
               ;; `hyperdrive-create', but perhaps it would be good to add a function which wraps
               ;; `hyperdrive-create' and returns either an existing hyperdrive or a new one?
               (hyperdrive (pcase host
                             ;; FIXME: Duplicate hyperdrive (one has domain and nothing else)
                             ((rx ".") ; Assume host is a DNSLink domain. See code for <https://github.com/RangerMauve/hyper-sdk#sdkget>.
                              (when (and (>= emacs-major-version 29)
                                         (textsec-suspicious-p host 'domain))
                                ;; Check DNSLink domains for suspicious characters; don't bother
                                ;; checking public keys since they're not recognizable anyway.
                                (unless (y-or-n-p
	                                 (format "Suspicious domain: %s; continue anyway?" host))
                                  (user-error "Suspicious domain %s" host)))
                              (hyperdrive-create :domains (list host)))
                             (_  ;; Assume host is a public-key
                              (or (gethash host hyperdrive-hyperdrives)
                                  (hyperdrive-create :public-key host)))))
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

(defun hyperdrive-entry-equal (a b)
  "Return non-nil if hyperdrive entries A and B are equal.
Compares only public key and path."
  (pcase-let (((cl-struct hyperdrive-entry (path a-path)
                          (hyperdrive (cl-struct hyperdrive (public-key a-key))))
               a)
              ((cl-struct hyperdrive-entry (path b-path)
                          (hyperdrive (cl-struct hyperdrive (public-key b-key))) )
               b))
    (and (equal a-path b-path)
         (equal a-key b-key))))

(defun hyperdrive-entry-latest (entry)
  "Return ENTRY at its hyperdrive's latest version, or nil."
  (hyperdrive-entry-at nil entry))

(defun hyperdrive--entry-version-range-key (entry)
  "Return URI-encoded URL for ENTRY without protocol, version, target, or face.
Intended to be used as hash table key in `hyperdrive-version-ranges'."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
               (version-less (hyperdrive-entry-create :hyperdrive hyperdrive :path path :encode t)))
    (hyperdrive--format-entry-url version-less :host-format '(public-key) :with-protocol nil
                                  :with-help-echo nil :with-target nil :with-faces nil)))

;; TODO: Add tests for version range functions
(defun hyperdrive-entry-version-ranges (entry)
  "Return version ranges for ENTRY."
  (gethash (hyperdrive--entry-version-range-key entry) hyperdrive-version-ranges))

(gv-define-setter hyperdrive-entry-version-ranges (ranges entry)
  `(progn
     (setf (gethash (hyperdrive--entry-version-range-key ,entry) hyperdrive-version-ranges) ,ranges)
     (persist-save 'hyperdrive-version-ranges)))

(defun hyperdrive-purge-version-ranges (hyperdrive)
  "Purge all version range data for HYPERDRIVE."
  (maphash (lambda (key _val)
             ;; NOTE: The KEY starts with the key and ends with a path, so we compare as prefix.
             (when (string-prefix-p (hyperdrive-public-key hyperdrive) key)
               (remhash key hyperdrive-version-ranges)))
           hyperdrive-version-ranges)
  (persist-save 'hyperdrive-version-ranges))

(defun hyperdrive-entry-version-range (entry)
  "Return the version range containing ENTRY.
Returns nil when ENTRY is not known to exist at its version."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive (version entry-version)) entry)
               (version (or entry-version (hyperdrive-latest-version hyperdrive)))
               (ranges (hyperdrive-entry-version-ranges entry)))
    (when ranges
      (cl-find-if (pcase-lambda (`(,range-start . ,(map (:range-end range-end))))
                    (<= range-start version range-end))
                  ranges))))

(defun hyperdrive-entry-exists-p (entry)
  "Return status of ENTRY's existence at its version.

- t       :: ENTRY is known to exist.
- nil     :: ENTRY is known to not exist.
- unknown :: ENTRY is not known to exist.

Does not make a request to the gateway; checks the cached value
in `hyperdrive-version-ranges'."
  (if-let ((range (hyperdrive-entry-version-range entry)))
      (pcase-let ((`(,_range-start . ,(map (:existsp existsp))) range))
        existsp)
    'unknown))

(defun hyperdrive-entry-version-ranges-no-gaps (entry)
  "Return ranges alist for ENTRY with no gaps in history.
Returned newly-constructed alist where each range-end is always
1- the following range-start.  Each gap is filled with a cons cell
whose car is the range start and whose cdr is a plist with a
numerical :RANGE-END and :EXISTSP set to 'UNKNOWN:

\(RANGE-START . (:RANGE-END RANGE-END :EXISTS 'UNKNOWN))

When the final range's range-end is less than ENTRY's
hyperdrive's latest-version slot, the final gap is filled."
  (let ((ranges '())
        (previous-range-end 0))
    (pcase-dolist (`(,range-start . ,(map (:range-end range-end) (:existsp existsp))) (hyperdrive-entry-version-ranges entry))
      ;; If hyperdrive-entry-version-ranges returns nil, this whole loop will be skipped.
      (let ((next-range-start (1+ previous-range-end)))
        (when (> range-start next-range-start)
          ;; Insert an "unknown" gap range
          (push `(,next-range-start . (:range-end ,(1- range-start) :existsp unknown)) ranges))
        (push `(,range-start . (:range-end ,range-end :existsp ,existsp)) ranges)
        (setf previous-range-end range-end)))
    (pcase-let* ((final-known-range (car ranges))
                 (`(,_range-start . ,(map (:range-end final-known-range-end))) final-known-range)
                 (latest-version (hyperdrive-latest-version (hyperdrive-entry-hyperdrive entry))))
      (unless final-known-range-end
        (setf final-known-range-end 0))
      (when (< final-known-range-end latest-version)
        ;; Insert possible final gap between latest known range and hyperdrive's latest-version
        (push `(,(1+ final-known-range-end) . (:range-end ,latest-version , :existsp unknown)) ranges)))
    (nreverse ranges)))

(defun hyperdrive-entry-previous (entry)
  "Return ENTRY at its hyperdrive's previous version, or nil.
If ENTRY is a directory, return a copy with decremented version."
  (if (hyperdrive--entry-directory-p entry)
      (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path version) entry)
                   (version (or version (hyperdrive-latest-version hyperdrive))))
        (when (> version 1)
          (hyperdrive-entry-create :hyperdrive hyperdrive :path path :version (1- version))))
    (when-let ((previous-entry (hyperdrive-entry-at (1- (car (hyperdrive-entry-version-range entry))) entry)))
      ;; Entry version is currently its range end, but it should be its version range start.
      (setf (hyperdrive-entry-version previous-entry) (car (hyperdrive-entry-version-range previous-entry)))
      previous-entry)))

(defun hyperdrive-entry-at (version entry)
  "Return ENTRY at its hyperdrive's VERSION, or nil if not found.
When VERSION is nil, return latest version of ENTRY."
  ;; Use `hyperdrive-copy-tree', because `copy-tree' doesn't work on
  ;; records/structs, and `copy-hyperdrive-entry' doesn't copy deeply,
  ;; and we need to be able to modify the `etc' alist of the copied
  ;; entry separately.
  (let ((entry (hyperdrive-copy-tree entry t)))
    (setf (hyperdrive-entry-version entry) version)
    (condition-case err
        (hyperdrive-fill entry :then 'sync)
      (plz-error
       (pcase (plz-response-status (plz-error-response (caddr err)))
         ;; FIXME: If plz-error is a curl-error, this block will fail.
         (404 nil)
         (_ (signal (car err) (cdr err))))))))

(cl-defun hyperdrive-entry-next (entry)
  "Return unfilled ENTRY at its hyperdrive's next version.

If next version is known nonexistent, return nil.
If next version's existence is unknown, return \\+`unknown'.
If ENTRY's version is nil, return value is `eq' to ENTRY.

Sends a request to the gateway for hyperdrive's latest version."
  (unless (hyperdrive-entry-version entry)
    ;; ENTRY's version is nil: return ENTRY.
    (cl-return-from hyperdrive-entry-next entry))

  ;; ENTRY's version is not nil.
  (let ((next-entry (hyperdrive-copy-tree entry t))
        (latest-version (hyperdrive-fill-latest-version
                         (hyperdrive-entry-hyperdrive entry))))

    ;; ENTRY version is the latest version: return ENTRY with nil version.
    (when (eq latest-version (hyperdrive-entry-version entry))
      (setf (hyperdrive-entry-version next-entry) nil)
      (cl-return-from hyperdrive-entry-next next-entry))

    ;; ENTRY is a directory: increment the version number by one.
    (when (hyperdrive--entry-directory-p entry)
      (cl-incf (hyperdrive-entry-version next-entry))
      (cl-return-from hyperdrive-entry-next next-entry))

    ;; ENTRY is a file...
    (pcase-let* ((`(,_range-start . ,(map (:range-end range-end))) (hyperdrive-entry-version-range entry))
                 (next-range-start (1+ range-end))
                 ((map (:existsp next-range-existsp) (:range-end next-range-end))
                  ;; TODO: If cl struct copiers are extended like this:
                  ;;       https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00797.html
                  ;;       replace following sexp with
                  ;;       (hyperdrive-entry-version-range (hyperdrive-entry-copy :version next-range-start))
                  (map-elt (hyperdrive-entry-version-ranges-no-gaps entry) next-range-start)))
      ;; ENTRY is in the last version range: return ENTRY with nil version.
      (when (eq latest-version range-end)
        (setf (hyperdrive-entry-version next-entry) nil)
        (cl-return-from hyperdrive-entry-next next-entry))

      ;; Check existence of ENTRY's next version range...
      (pcase-exhaustive next-range-existsp
        ('t
         (setf (hyperdrive-entry-version next-entry)
               (if (eq next-range-end latest-version)
                   ;; This is the latest version: remove version number.
                   nil
                 next-range-start))
         next-entry)
        ('nil nil)
        ('unknown 'unknown)))))

(declare-function hyperdrive-history "hyperdrive-history")
(cl-defun hyperdrive-open (entry &key then recurse (createp t))
  "Open hyperdrive ENTRY.
If RECURSE, proceed up the directory hierarchy if given path is
not found. THEN is a function to pass to the handler which will
be called with no arguments in the buffer opened by the handler.
When a writable ENTRY is not found and CREATEP is non-nil, create
a new buffer for ENTRY."
  (declare (indent defun))
  ;; TODO: Add `find-file'-like interface. See <https://todo.sr.ht/~ushin/ushin/16>
  ;; FIXME: Some of the synchronous filling functions we've added now cause this to be blocking,
  ;; which is very noticeable when a file can't be loaded from the gateway and eventually times out.
  (let ((hyperdrive (hyperdrive-entry-hyperdrive entry)))
    (hyperdrive-fill entry
      :then (lambda (entry)
              (pcase-let* (((cl-struct hyperdrive-entry type) entry)
                           (handler (alist-get type hyperdrive-type-handlers nil nil #'string-match-p)))
                (unless (hyperdrive--entry-directory-p entry)
                  ;; No need to fill latest version for directories,
                  ;; since we do it in `hyperdrive--fill' already.
                  (hyperdrive-fill-latest-version hyperdrive))
                (hyperdrive-persist hyperdrive)
                (funcall (or handler #'hyperdrive-handler-default) entry :then then)))
      :else (lambda (err)
              (cl-labels ((not-found-action
                            () (if recurse
                                   (hyperdrive-open (hyperdrive-parent entry) :recurse t)
                                 (pcase (prompt)
                                   ('history (hyperdrive-history entry))
                                   ('up (hyperdrive-open (hyperdrive-parent entry)))
                                   ('recurse (hyperdrive-open (hyperdrive-parent entry) :recurse t)))))
                          (prompt
                            () (pcase-exhaustive
                                   (read-answer (format "URL not found: \"%s\". " (hyperdrive-entry-url entry))
                                                '(("history" ?h "open version history")
                                                  ("up" ?u "open parent directory")
                                                  ("recurse" ?r "go up until a directory is found")
                                                  ("exit" ?q "exit")))
                                 ("history" 'history)
                                 ("up" 'up)
                                 ("recurse" 'recurse)
                                 ("exit" nil))))
                (pcase (plz-response-status (plz-error-response err))
                  ;; FIXME: If plz-error is a curl-error, this block will fail.
                  (404 ;; Path not found.
                   (cond
                    ((equal (hyperdrive-entry-path entry) "/")
                     ;; Root directory not found: Drive has not been
                     ;; loaded locally, and no peers are found seeding it.
                     (hyperdrive-message "No peers found for %s" (hyperdrive-entry-url entry)))
                    ((and createp
                          (not (hyperdrive--entry-directory-p entry))
                          (hyperdrive-writablep hyperdrive)
                          (not (hyperdrive-entry-version entry)))
                     ;; Entry is a writable file: create a new buffer
                     ;; that will be saved to its path.
                     (if-let ((buffer (get-buffer (hyperdrive--entry-buffer-name entry))))
                         ;; Buffer already exists: likely the user deleted the entry
                         ;; without killing the buffer.  Switch to the buffer and
                         ;; alert the user that the entry no longer exists.
                         (progn
                           (switch-to-buffer buffer)
                           (message "Entry no longer exists!  %s" (hyperdrive-entry-description entry)))
                       ;; Make and switch to new buffer.
                       (switch-to-buffer (hyperdrive--get-buffer-create entry))))
                    (t
                     ;; Hyperdrive entry is not writable: prompt for action.
                     (not-found-action))))
                  (500 ;; Generic error, likely a mistyped URL
                   (hyperdrive-message "Generic hyper-gateway status 500 error. Is this URL correct? %s"
                                       (hyperdrive-entry-url entry)))
                  (_ (hyperdrive-message "Unable to load URL \"%s\": %S"
                                         (hyperdrive-entry-url entry) err))))))))

(cl-defun hyperdrive-fill (entry &key queue then else)
  "Fill ENTRY's metadata and call THEN.
If THEN is `sync', return the filled entry and ignore ELSE.
Otherwise, make request asynchronously and call THEN with the
filled entry; or if request fails, call ELSE (which is passed to
`hyperdrive-api', which see.  If QUEUE, make the fill request in
the given `plz-queue'"
  (declare (indent defun))
  (unless else
    ;; Binding this in the function argument form causes a spurious
    ;; lint warning about a docstring being too long, so we do this
    ;; here instead.
    (setf else (lambda (plz-error)
                 (pcase (plz-error-message plz-error)
                   ((or (rx "queue cleared; request canceled.")
                        "curl process killed")
                    ;; Don't message when the queue was cleared
                    ;; (e.g. if the user reverted too quickly).
                    nil)
                   (_
                    (hyperdrive-message
                     (format "hyperdrive-fill: error: %S" plz-error)))))))
  (pcase then
    ('sync (condition-case err
               (hyperdrive--fill entry
                                 (plz-response-headers
                                  (hyperdrive-api 'head (hyperdrive-entry-url entry)
                                    :as 'response
                                    :then 'sync
                                    :noquery t)))
             (plz-error
              (pcase (plz-response-status (plz-error-response (caddr err)))
                ;; FIXME: If plz-error is a curl-error, this block will fail.
                (404 ;; Entry doesn't exist at this version: update range data.
                 (hyperdrive-update-nonexistent-version-range entry)))
              ;; Re-signal error for, e.g. `hyperdrive-entry-at'.
              (signal (car err) (cdr err)))))
    (_ (hyperdrive-api 'head (hyperdrive-entry-url entry)
         :queue queue
         :as 'response
         :then (lambda (response)
                 (funcall then (hyperdrive--fill entry (plz-response-headers response))))
         :else (lambda (&rest args)
                 (when (hyperdrive-entry-version entry)
                   ;; If request is canceled, the entry may not have a version.
                   (hyperdrive-update-nonexistent-version-range entry))
                 (apply else args))
         :noquery t))))

(defun hyperdrive--fill (entry headers)
  "Fill ENTRY and its hyperdrive from HEADERS.

The following ENTRY slots are filled:
- type
- mtime
- size
- hyperdrive (from persisted value if it exists)

The following ENTRY hyperdrive slots are filled:
- public-key
- writablep (when headers include Allow)
- domains (merged with current persisted value)

Returns filled ENTRY."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive) entry)
               ((cl-struct hyperdrive writablep domains) hyperdrive)
               ((map link content-length content-type etag last-modified allow) headers)
               ;; If URL hostname was a DNSLink domain, entry doesn't yet have a public-key slot.
               (public-key (progn
                             (string-match hyperdrive--public-key-re link)
                             (match-string 1 link)))
               (persisted-hyperdrive (gethash public-key hyperdrive-hyperdrives))
               (domain (car domains)))
    (when last-modified
      (setf last-modified (encode-time (parse-time-string last-modified))))
    (when (and allow (eq 'unknown writablep))
      (setf (hyperdrive-writablep hyperdrive) (string-match-p "PUT" allow)))
    (setf (hyperdrive-entry-size entry) (when content-length
                                          (ignore-errors
                                            (cl-parse-integer content-length)))
          (hyperdrive-entry-type entry) content-type
          (hyperdrive-entry-mtime entry) last-modified)
    (if persisted-hyperdrive
        (progn
          ;; Ensure that entry's hyperdrive is the persisted
          ;; hyperdrive, since it may be used later as part of a
          ;; `hyperdrive-version-ranges' key and compared using `eq'.
          ;; Also, we want the call to `hyperdrive--fill-latest-version'
          ;; below to update the persisted hyperdrive.
          (setf (hyperdrive-entry-hyperdrive entry) persisted-hyperdrive)
          (when domain
            ;; The previous call to hyperdrive-entry-url may not have retrieved
            ;; the persisted hyperdrive if we had only a domain but no public-key.
            (cl-pushnew domain (hyperdrive-domains (hyperdrive-entry-hyperdrive entry)) :test #'equal)))
      (setf (hyperdrive-public-key hyperdrive) public-key))
    (if (and (hyperdrive--entry-directory-p entry)
             (null (hyperdrive-entry-version entry)))
        ;; Version-less directory HEAD/GET request ETag header always have the
        ;; hyperdrive's latest version. We don't currently store
        ;; version ranges for directories (since they don't
        ;; technically have versions in hyperdrive).
        (hyperdrive--fill-latest-version hyperdrive headers)
      ;; File HEAD/GET request ETag header does not retrieve the
      ;; hyperdrive's latest version, so `hyperdrive-update-existent-version-range'
      ;; will not necessarily fill in the entry's last range.
      (hyperdrive-update-existent-version-range entry (string-to-number etag)))
    entry))

(defun hyperdrive-fill-latest-version (hyperdrive)
  "Synchronously fill the latest version slot in HYPERDRIVE.
Returns the latest version number."
  (pcase-let (((cl-struct plz-response headers)
               (hyperdrive-api
                 'head (hyperdrive-entry-url
                        (hyperdrive-entry-create
                         :hyperdrive hyperdrive :path "/"))
                 :as 'response)))
    (hyperdrive--fill-latest-version hyperdrive headers)))

(defun hyperdrive--fill-latest-version (hyperdrive headers)
  "Fill the latest version slot in HYPERDRIVE from HEADERS.
HEADERS must from a HEAD/GET request to a directory, as only
those requests return the correct ETag header.
Returns the latest version number."
  ;; TODO: Update relevant buffers when hyperdrive latest version
  ;; updates, at the least describe-hyperdrive buffers.
  ;; TODO: Consider updating version range here. First check all the
  ;; places where this function is called. Better yet, update
  ;; `hyperdrive-version-ranges' (and `hyperdrive-hyperdrives'?) in a
  ;; lower-level function, perhaps a wrapper for `hyperdrive-api'?
  (setf (hyperdrive-latest-version hyperdrive) (string-to-number (map-elt headers 'etag))))

;; TODO: Consider using symbol-macrolet to simplify place access.

(defun hyperdrive-update-existent-version-range (entry range-start)
  "Update the version range for ENTRY which exists at its version.
Sets the range keyed by RANGE-START to a plist whose :range-end
value is ENTRY's version.

For the format of each version range, see `hyperdrive-version-ranges'.

Returns the ranges cons cell for ENTRY."
  (cl-check-type range-start integer)
  (unless (hyperdrive--entry-directory-p entry)
    (pcase-let* ((ranges (hyperdrive-entry-version-ranges entry))
                 (range (map-elt ranges range-start))
                 ((map (:range-end old-range-end)) range)
                 ((cl-struct hyperdrive-entry hyperdrive version) entry)
                 (range-end (or version (hyperdrive-latest-version hyperdrive))))
      (unless (and old-range-end (> old-range-end range-end))
        ;; If there already exists a longer existent range in
        ;; `hyperdrive-version-ranges', there's nothing to do.
        (setf (plist-get range :existsp) t
              (plist-get range :range-end) range-end
              (map-elt ranges range-start) range
              (hyperdrive-entry-version-ranges entry) (cl-sort ranges #'< :key #'car))))))

(defun hyperdrive-update-nonexistent-version-range (entry)
  "Update the version range for ENTRY which doesn't exist at its version.
Checks for nonexistent previous or next ranges, to combine them
into one contiguous nonexistent range.

For the format of each version range, see `hyperdrive-version-ranges'.

Returns the ranges cons cell for ENTRY."
  (unless (or (hyperdrive--entry-directory-p entry)
              ;; If there already exists a nonexistent range in
              ;; `hyperdrive-version-ranges', there's nothing to do.
              (hyperdrive-entry-version-range entry)
              ;; Don't store ranges for entries which have never existed.
              (not (hyperdrive-entry-version-ranges entry)))
    (pcase-let* ((ranges (hyperdrive-entry-version-ranges entry))
                 ((cl-struct hyperdrive-entry hyperdrive path version) entry)
                 (version (or version (hyperdrive-latest-version hyperdrive)))
                 (previous-range (hyperdrive-entry-version-range
                                  (hyperdrive-entry-create :hyperdrive hyperdrive :path path :version (1- version))))
                 (`(,previous-range-start . ,(map (:existsp previous-exists-p))) previous-range)
                 (next-range (hyperdrive-entry-version-range
                              (hyperdrive-entry-create :hyperdrive hyperdrive :path path :version (1+ version))))
                 (`(,next-range-start . ,(map (:existsp next-exists-p) (:range-end next-range-end))) next-range)
                 (range-start (if (and previous-range (null previous-exists-p))
                                  ;; Extend previous nonexistent range
                                  previous-range-start
                                version))
                 (range-end (if (and next-range (null next-exists-p))
                                ;; Extend next nonexistent range
                                next-range-end
                              version)))
      ;; Delete next range if it's contiguous with current range.
      (when (and next-range (null next-exists-p))
        (setf ranges (map-delete ranges next-range-start)))
      (setf (map-elt ranges range-start) `(:existsp nil :range-end ,range-end)
            (hyperdrive-entry-version-ranges entry) (cl-sort ranges #'< :key #'car)))))

(cl-defun hyperdrive-fill-version-ranges (entry &key then)
  "Asynchronously fill in versions ranges for ENTRY and call THEN.
First fill latest version of ENTRY's hyperdrive.  Then recurse
backward through some unknown ranges and fill them.  Once all
requests return, call THEN with no arguments."
  ;; TODO: Limit the number of recursive calls made.
  (declare (indent defun))
  ;; Filling drive's latest version lets us display the full history,
  ;; and it ensures that the final range is not unknown.
  (hyperdrive-fill-latest-version (hyperdrive-entry-hyperdrive entry))
  (let* ((ranges-no-gaps (hyperdrive-entry-version-ranges-no-gaps entry))
         (ranges-to-fill
          (cl-delete-if-not
           ;; Select certain unknown ranges to be filled. Unknown
           ;; ranges are filled by requesting the version at its
           ;; range-end. The entry at the range-end of an unknown
           ;; ranges which is followed by a nonexistent entry is
           ;; likely to also be nonexistent. By only attempting to
           ;; fill unknown ranges which are either followed by a
           ;; existent range or are themselves the final range, we
           ;; minimize the number of unnecessary requests.
           (pcase-lambda (`(,_range-start . ,(map (:existsp existsp) (:range-end range-end))))
             (and (eq 'unknown existsp)
                  (if-let ((next-range (map-elt ranges-no-gaps (1+ range-end))))
                      ;; If next range exists, fill it.
                      (eq t (map-elt next-range :existsp))
                    ;; This is the final range: fill it.
                    t)))
           ranges-no-gaps))
         queue)
    (if ranges-to-fill
        (progn
          ;; TODO: When `plz' lets us handle errors in the queue finalizer, add that here.
          (setf queue (make-plz-queue :limit hyperdrive-queue-size :finally then))
          (cl-labels ((fill-recursively (unknown-entry)
                        ;; NOTE: `fill-recursively' is recursive logically but
                        ;; not technically, because each call is in the async callback.
                        ;; Fill entry at its version, then if its previous
                        ;; version is unknown, recurse on previous version.
                        (hyperdrive-fill unknown-entry
                          ;; `hyperdrive-fill' is only used here for updating
                          ;; `hyperdrive-version-ranges'. The copied entry is thrown away.
                          :then (lambda (filled-entry)
                                  ;; Don't use `hyperdrive-entry-previous' here, since it makes a sync request
                                  (pcase-let ((`(,range-start . ,_plist) (hyperdrive-entry-version-range filled-entry)))
                                    (setf (hyperdrive-entry-version filled-entry) (1- range-start))
                                    (when (eq 'unknown (hyperdrive-entry-exists-p filled-entry))
                                      ;; Recurse backward through history, filling unknown
                                      ;; entries. Stop recursing at known nonexistent entry.
                                      (fill-recursively filled-entry))))
                          :else (lambda (err)
                                  (pcase (plz-response-status (plz-error-response err))
                                    ;; FIXME: If plz-error is a curl-error, this block will fail.
                                    ;; TODO: How to handle entries which have never been known
                                    ;; existent. From a UI perspective, the history buffer
                                    ;; should display the versions at which the entry is known
                                    ;; non-existent. However, we don't want to store loads of
                                    ;; non-existent entries in `hyperdrive-version-ranges'.
                                    (404 nil)
                                    (_ (signal (car err) (cdr err))))
                                  err)
                          :queue queue)))
            (pcase-dolist (`(,_range-start . ,(map (:range-end range-end))) ranges-to-fill)
              ;; TODO: Consider using async iterator instead (with `iter-defun' or `aio'?)
              (let ((range-end-entry (hyperdrive-copy-tree entry t)))
                (setf (hyperdrive-entry-version range-end-entry) range-end)
                (fill-recursively range-end-entry)))))
      (funcall then))))

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
               (metadata (condition-case err
                             (hyperdrive-api 'get (hyperdrive-entry-url entry)
                               :as (lambda ()
                                     (condition-case err
                                         (json-read)
                                       (json-error
                                        (hyperdrive-message "Error parsing JSON metadata file: %s"
                                                            (hyperdrive-entry-url entry)))
                                       (_ (signal (car err) (cdr err)))))
                               :noquery t)
                           (plz-error
                            (pcase (plz-response-status (plz-error-response (caddr err)))
                              ;; FIXME: If plz-error is a curl-error, this block will fail.
                              (404 nil)
                              (_ (signal (car err) (cdr err))))))))
    (setf (hyperdrive-metadata hyperdrive) metadata)
    (hyperdrive-persist hyperdrive)
    hyperdrive))

(cl-defun hyperdrive-delete (entry &key then else)
  "Delete ENTRY, then call THEN.
Call ELSE if request fails."
  (declare (indent defun))
  ;; TODO: update-version-ranges here.
  ;; TODO: `hyperdrive--fill-latest-version' here.
  (hyperdrive-api 'delete (hyperdrive-entry-url entry)
    :then then :else else))

(cl-defun hyperdrive-purge-no-prompt (hyperdrive &key then else)
  "Purge all data corresponding to HYPERDRIVE, then call THEN with response.

- HYPERDRIVE file content and metadata managed by hyper-gateway
  in `hyperdrive-storage-location'
- hash table entry for HYPERDRIVE in `hyperdrive-hyperdrives'
- hash table entries for HYPERDRIVE in `hyperdrive-version-ranges'

Call ELSE if request fails."
  (declare (indent defun))
  (hyperdrive-api 'delete (hyperdrive-entry-url (hyperdrive-entry-create :hyperdrive hyperdrive))
    :as 'response
    :then (lambda (response)
            (hyperdrive-persist hyperdrive :purge t)
            (hyperdrive-purge-version-ranges hyperdrive)
            (funcall then response))
    :else else))

(cl-defun hyperdrive-write (entry &key body then else queue)
  "Write BODY to hyperdrive ENTRY's URL."
  (declare (indent defun))
  (hyperdrive--write (hyperdrive-entry-url entry)
    :body body :then then :else else :queue queue))

(cl-defun hyperdrive-entry-description (entry &key (format-path 'path) (with-version t))
  "Return description for ENTRY.
When ENTRY has a non-nil VERSION slot, include it.  Returned
string looks like:

  FORMAT-PATH [HOST] (version:VERSION)

When FORMAT-PATH is `path', use full path to entry.  When
FORMAT-PATH is `name', use only last part of path, as in
`file-name-non-directory'.

When WITH-VERSION or ENTRY's version is nil, omit (version:VERSION)."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive version path name) entry)
               (handle (hyperdrive--format-host hyperdrive
                                                :format hyperdrive-default-host-format
                                                :with-label t)))
    (propertize (concat (format "[%s] " handle)
                        (pcase format-path
                          ('path (url-unhex-string path))
                          ('name name))
                        (when (and version with-version)
                          (format " (version:%s)" version)))
                'help-echo (hyperdrive-entry-url entry))))

(cl-defun hyperdrive--format-entry-url
    (entry &key (host-format '(public-key domain))
           (with-protocol t) (with-help-echo t) (with-target t) (with-faces t))
  "Return ENTRY's URL.
Returns URL formatted like:

  hyper://HOST-FORMAT/PATH/TO/FILE

HOST-FORMAT is passed to `hyperdrive--format-host', which see.  If
WITH-PROTOCOL, \"hyper://\" is prepended.  If WITH-HELP-ECHO,
propertize string with `help-echo' property showing the entry's
full URL.  When WITH-FACES is nil, don't add face text properties.
If WITH-TARGET, append the ENTRY's target, stored in its :etc
slot.  When ENTRY has non-nil `version' slot, include version
number in URL.

Note that, if HOST-FORMAT includes values other than `public-key'
and `domain', the resulting URL may not be a valid hyperdrive
URL."
  ;; NOTE: Entries may have only a domain, not a public key yet, so we
  ;; include `domain' in HOST-FORMAT's default value.  The public key
  ;; will be filled in later.
  (pcase-let* (((cl-struct hyperdrive-entry path version etc)
                entry)
               (protocol (when with-protocol
                           "hyper://"))
               (host (hyperdrive--format-host (hyperdrive-entry-hyperdrive entry)
                                              :format host-format :with-faces with-faces))
               (version-part (and version (format "/$/version/%s" version)))
               ((map target) etc)
               (target-part (when (and with-target target)
                              (concat "#" target)))
               (url (concat protocol host version-part path target-part)))
    (if with-help-echo
        (propertize url
                    'help-echo (hyperdrive--format-entry-url
                                entry :with-protocol t :host-format '(public-key domain)
                                :with-help-echo nil :with-target with-target :with-faces with-faces))
      url)))

(cl-defun hyperdrive--format-host (hyperdrive &key format with-label (with-faces t))
  "Return HYPERDRIVE's formatted hostname, or nil.
FORMAT should be a list of symbols; see
`hyperdrive-default-host-format' for choices.  If the specified
FORMAT is not available, returns nil.  If WITH-LABEL, prepend a
label for the kind of format used (e.g. \"petname:\").
When WITH-FACES is nil, don't add face text properties."
  (pcase-let* (((cl-struct hyperdrive petname public-key domains seed
                           (metadata (map name)))
                hyperdrive))
    (cl-flet ((fmt (string label face)
                (concat (when with-label
                          label)
                        (if with-faces
                            (propertize string 'face face)
                          string))))
      (cl-loop for f in format
               when (pcase f
                      ((and 'petname (guard petname))
                       (fmt petname "petname:" 'hyperdrive-petname))
                      ((and 'nickname (guard name))
                       (fmt name "nickname:" 'hyperdrive-nickname))
                      ((and 'domain (guard (car domains)))
                       ;; TODO: Handle the unlikely case that a drive has multiple domains.
                       (fmt (car domains) "domain:" 'hyperdrive-domain))
                      ((and 'seed (guard seed))
                       (fmt seed "seed:" 'hyperdrive-seed))
                      ((and 'short-key (guard public-key))
                       ;; TODO: Consider adding a help-echo with the full key.
                       (fmt (concat (substring public-key 0 6) "…") "public-key:" 'hyperdrive-public-key))
                      ((and 'public-key (guard public-key))
                       (fmt public-key "public-key:" 'hyperdrive-public-key)))
               return it))))

;;;; Reading from the user

(cl-defun hyperdrive-complete-hyperdrive (&key predicate force-prompt)
  "Return hyperdrive for current entry when it matches PREDICATE.

With FORCE-PROMPT or when current hyperdrive does not match
PREDICATE, return a hyperdrive selected with completion.  In this
case, when PREDICATE, only offer hyperdrives matching it."
  (unless predicate
    ;; cl-defun default value doesn't work when nil predicate value is passed in.
    (setf predicate #'always))
  (let ((current-hyperdrive (when hyperdrive-current-entry
                              (hyperdrive-entry-hyperdrive hyperdrive-current-entry))))
    (if (and (not force-prompt)
             hyperdrive-current-entry
             (funcall predicate current-hyperdrive))
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
            (hyperdrive-user-error "No such hyperdrive.  Use `hyperdrive-new' to create a new one"))))))

(cl-defun hyperdrive--format-hyperdrive (hyperdrive)
  "Return HYPERDRIVE formatted for completion."
  (string-trim
   (cl-loop for format in '(petname nickname domain seed short-key)
            when (hyperdrive--format-host hyperdrive :format (list format) :with-label t)
            concat (concat it "  "))))

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
INITIAL-INPUT-NUMBER is converted to a string and passed to
`read-string', which see."
  (let* ((prompt (or prompt "Version number in «%s» (leave blank for latest version)"))
         ;; Don't use read-number since it cannot return nil.
         (version (read-string
                   (format-prompt prompt nil (hyperdrive--format-hyperdrive hyperdrive))
                   (when initial-input-number (number-to-string initial-input-number))
                   'hyperdrive--version-history)))
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
Prompts with PROMPT.  Defaults to current entry if it exists."
  (let ((default (when hyperdrive-current-entry
                   (hyperdrive-entry-url hyperdrive-current-entry))))
    (string-trim (read-string (format-prompt prompt default) nil 'hyperdrive--url-history default))))

(defvar hyperdrive--name-history nil
  "Minibuffer history of `hyperdrive-read-name'.")

(cl-defun hyperdrive-read-name (&key prompt initial-input default)
  "Wrapper for `read-string' with common history.
Prompts with PROMPT and DEFAULT, according to `format-prompt'.
DEFAULT and INITIAL-INPUT are passed to `read-string' as-is."
  (read-string (format-prompt prompt default) initial-input 'hyperdrive--name-history default))

(cl-defun hyperdrive-put-metadata (hyperdrive &key then)
  "Put HYPERDRIVE's metadata into the appropriate file, then call THEN."
  (declare (indent defun))
  (let ((entry (hyperdrive-entry-create :hyperdrive hyperdrive
                                        :path "/.well-known/host-meta.json")))
    (hyperdrive-write entry :body (json-encode (hyperdrive-metadata hyperdrive))
      :then then)
    hyperdrive))

(cl-defun hyperdrive-persist (hyperdrive &key purge)
  "Persist HYPERDRIVE in `hyperdrive-hyperdrives'.
With PURGE, delete hash table entry for HYPERDRIVE."
  ;; TODO: Make separate function for purging persisted data.
  (if purge
      (remhash (hyperdrive-public-key hyperdrive) hyperdrive-hyperdrives)
    (puthash (hyperdrive-public-key hyperdrive) hyperdrive hyperdrive-hyperdrives))
  (persist-save 'hyperdrive-hyperdrives))

(defun hyperdrive-seed-url (seed)
  "Return URL to hyperdrive known as SEED, or nil if it doesn't exist.
That is, if the SEED has been used to create a local
hyperdrive."
  (condition-case err
      (pcase (hyperdrive-api 'get (concat "hyper://localhost/?key=" (url-hexify-string seed))
               :as 'response :noquery t)
        ((and (pred plz-response-p)
              response
              (guard (= 200 (plz-response-status response))))
         (plz-response-body response)))
    (plz-error (if (= 400 (plz-response-status (plz-error-response (caddr err))))
                   ;; FIXME: If plz-error is a curl-error, this block will fail.
                   nil
                 (signal (car err) (cdr err))))))

;;;###autoload
(defun hyperdrive-by-slot (slot value)
  "Return persisted hyperdrive struct whose SLOT matches VALUE.
Otherwise, return nil.  SLOT may be one of

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

;;;; Handlers

(declare-function hyperdrive--org-link-goto "hyperdrive-org")
(cl-defun hyperdrive-handler-default (entry &key then)
  "Load ENTRY's file into an Emacs buffer.
If then, then call THEN with no arguments.  Default handler."
  (hyperdrive-api 'get (hyperdrive-entry-url entry)
    :noquery t
    :as (lambda ()
          (pcase-let* (((cl-struct hyperdrive-entry hyperdrive version etc) entry)
                       ((map target) etc)
                       (response-buffer (current-buffer)))
            (with-current-buffer (hyperdrive--get-buffer-create entry)
              ;; TODO: Don't reload if we're jumping to a link on the
              ;; same page (but ensure that reverting still works).
              (if (buffer-modified-p)
                  (hyperdrive-message "Buffer modified: %S" (current-buffer))
                (with-silent-modifications
                  (erase-buffer)
                  (insert-buffer-substring response-buffer))
                (setf buffer-undo-list nil
                      buffer-read-only (or (not (hyperdrive-writablep hyperdrive)) version))
                (set-buffer-modified-p nil)
                (set-visited-file-modtime (current-time))
                (goto-char (point-min)))
              ;; TODO: Option to defer showing buffer.
              ;; It seems that `pop-to-buffer' is moving point, even
              ;; though it shouldn't, so we call it here, before going
              ;; to a link target.
              (pop-to-buffer (current-buffer))
              (when target
                (pcase major-mode
                  ('org-mode
                   (require 'hyperdrive-org)
                   (hyperdrive--org-link-goto target))
                  ('markdown-mode
                   ;; TODO: Handle markdown link
                   )))
              (when then
                (funcall then)))))))

(cl-defun hyperdrive-handler-streamable (entry &key _then)
  ;; TODO: Is there any reason to not pass THEN through?
  ;; FIXME: Opening a streamable entry from a hyperdrive-dir buffer
  ;; buries the -dir buffer.
  "Stream ENTRY."
  (hyperdrive-message (format "Streaming %s..." (hyperdrive--format-entry-url entry)))
  (pcase-let ((`(,command . ,args)
               (split-string hyperdrive-stream-player-command)))
    (apply #'start-process "hyperdrive-stream-player"
           nil command (cl-substitute (hyperdrive--httpify-url
                                       (hyperdrive-entry-url entry))
                                      "%s" args :test #'equal))))

(declare-function hyperdrive-dir-handler "hyperdrive-dir")
(cl-defun hyperdrive-handler-json (entry &key then)
  "Show ENTRY.
THEN is passed to other handlers, which see.  If ENTRY is a
directory (if its URL ends in \"/\"), pass to
`hyperdrive-dir-handler'.  Otherwise, open with
`hyperdrive-handler-default'."
  (if (hyperdrive--entry-directory-p entry)
      (hyperdrive-dir-handler entry :then then)
    (hyperdrive-handler-default entry :then then)))

(cl-defun hyperdrive-handler-html (entry &key then)
  "Show ENTRY, where ENTRY is an HTML file.
If `hyperdrive-render-html' is non-nil, render HTML with
`shr-insert-document', then calls THEN if given.  Otherwise, open
with `hyperdrive-handler-default'."
  (if hyperdrive-render-html
      (progn
        (eww (hyperdrive-entry-url entry))
        ;; Set `hyperdrive-current-entry' and use `hyperdrive-mode'
        ;; for remapped keybindings for, e.g., `hyperdrive-up'.
        (setq-local hyperdrive-current-entry entry)
        (hyperdrive-mode)
        (when then
          (funcall then)))
    (hyperdrive-handler-default entry :then then)))

(cl-defun hyperdrive-handler-image (entry &key then)
  "Show ENTRY, where ENTRY is an image file.
Then calls THEN if given."
  (hyperdrive-handler-default
   entry :then (lambda ()
		 (image-mode)
		 (when then
		   (funcall then)))))

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
both point to the same content.

Affected by option `hyperdrive-reuse-buffers', which see."
  (let* ((buffer-name (hyperdrive--entry-buffer-name entry))
         (buffer
          (or (when (eq 'any-version hyperdrive-reuse-buffers)
                (cl-loop for buffer in (buffer-list)
                         when (hyperdrive--entry-buffer-p entry buffer)
                         return buffer))
              (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      (rename-buffer buffer-name)
      ;; NOTE: We do not erase the buffer because, e.g. the directory
      ;; handler needs to record point before it erases the buffer.
      (if (hyperdrive--entry-directory-p entry)
          (hyperdrive-dir-mode)
        (when hyperdrive-honor-auto-mode-alist
          ;; Inspired by https://emacs.stackexchange.com/a/2555/39549
          (let ((buffer-file-name (hyperdrive-entry-name entry)))
            (set-auto-mode))))
      (hyperdrive-mode)
      (setq-local hyperdrive-current-entry entry)
      (current-buffer))))

(defun hyperdrive--entry-buffer-p (entry buffer)
  "Return non-nil when BUFFER is visiting ENTRY."
  (and (buffer-local-value 'hyperdrive-mode buffer)
       (buffer-local-value 'hyperdrive-current-entry buffer)
       (hyperdrive-entry-equal entry
                               (buffer-local-value 'hyperdrive-current-entry buffer))))

(defun hyperdrive--buffer-for-entry (entry)
  "Return a predicate to match buffer against ENTRY"
  ;; TODO: This function is a workaround for bug#65797
  (lambda (buffer) (hyperdrive--entry-buffer-p entry buffer)))

(defun hyperdrive--entry-buffer-name (entry)
  "Return buffer name for ENTRY."
  (hyperdrive-entry-description entry :format-path 'name))

(defun hyperdrive--entry-directory-p (entry)
  "Return non-nil if ENTRY is a directory."
  (string-suffix-p "/" (hyperdrive-entry-path entry)))

(defun hyperdrive-message (message &rest args)
  "Call `message' with MESSAGE and ARGS, prefixing MESSAGE with \"Hyperdrive:\"."
  (apply #'message (concat "Hyperdrive: " message) args))

(defun hyperdrive-user-error (format &rest args)
  "Call `user-error' with FORMAT and ARGS, prefixing FORMAT with \"Hyperdrive:\"."
  (apply #'user-error (concat "Hyperdrive: " format) args))

(defun hyperdrive-insert-button (text &rest properties)
  "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
  ;; Inspired by package.el's `package-make-button'.
  (let ((button-text (if (display-graphic-p) text (concat "[" text "]")))
        (button-face (if (display-graphic-p) 'hyperdrive-button 'link)))
    (apply #'insert-text-button button-text 'face button-face 'follow-link t
           properties)))

(defun hyperdrive-copy-tree (tree &optional vecp)
  "Copy TREE like `copy-tree', but with VECP, works for records too."
  ;; TODO: Now that the new copy-tree behavior has been merged into Emacs,
  ;; remove this function once compat.el supports the new behavior.
  (if (consp tree)
      (let (result)
	(while (consp tree)
	  (let ((newcar (car tree)))
	    (if (or (consp (car tree)) (and vecp (or (vectorp (car tree)) (recordp (car tree)))))
		(setq newcar (hyperdrive-copy-tree (car tree) vecp)))
	    (push newcar result))
	  (setq tree (cdr tree)))
	(nconc (nreverse result)
               (if (and vecp (or (vectorp tree) (recordp tree))) (hyperdrive-copy-tree tree vecp) tree)))
    (if (and vecp (or (vectorp tree) (recordp tree)))
	(let ((i (length (setq tree (copy-sequence tree)))))
	  (while (>= (setq i (1- i)) 0)
	    (aset tree i (hyperdrive-copy-tree (aref tree i) vecp)))
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

(defun hyperdrive-expand-url (path &optional base)
  "Return a URL string of PATH expanded against current entry.
When BASE is non-nil, PATH will be expanded against BASE instead."
  (let* ((urlobj (url-generic-parse-url path))
         (defobj (url-generic-parse-url (or base (hyperdrive-entry-url hyperdrive-current-entry)))))
    ;; Destructively modify the URL object to give it the correct host and path.
    (url-default-expander urlobj defobj)
    (url-recreate-url urlobj)))

;;;; Utilities

(defun hyperdrive-time-greater-p (a b)
  "Return non-nil if time value A is greater than B."
  (not (time-less-p a b)))

(defun hyperdrive--clean-buffer (&optional buffer)
  "Remove all local variables, overlays, and text properties in BUFFER.
 When BUFFER is nil, act on current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (kill-all-local-variables t)
    (let ((inhibit-read-only t))
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (delete-overlay overlay))
      (set-text-properties (point-min) (point-max) nil))))

(provide 'hyperdrive-lib)
;;; hyperdrive-lib.el ends here
