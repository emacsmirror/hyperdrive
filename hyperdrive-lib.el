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

(declare-function h/mode "hyperdrive")
(declare-function h/dir-mode "hyperdrive-dir")

;;;; Errors

(define-error 'h/error "hyperdrive error")

(defun h/error (&rest args)
  "Like `error', but signals `hyperdrive-error'.
Passes ARGS to `format-message'."
  (signal 'h/error (list (apply #'format-message args))))

;;;; Structs

(cl-defstruct (hyperdrive-entry (:constructor he//create)
                                (:copier nil))
  "Represents an entry in a hyperdrive."
  (hyperdrive nil :documentation "The entry's hyperdrive.")
  ;; Rather than storing just the path and making a function to return
  ;; the name, we store the name as-is because, for one thing, the name
  ;; could theoretically contain a slash, and `file-name-nondirectory'
  ;; would return the wrong value in that case.
  (name nil :documentation "Decoded filename of entry (excluding leading slash).")
  (path nil :documentation "Decoded path (including leading slash).")
  (headers nil :documentation "HTTP headers from request.")
  (mtime nil :documentation "Last modified time.")
  (size nil :documentation "Size of file.")
  (version nil :documentation "Hyperdrive version specified in entry's URL.")
  (type nil :documentation "MIME type of the entry.")
  (etc nil :documentation "Alist for extra data about the entry."))

(cl-defstruct (hyperdrive (:constructor h/create)
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

(defun h/url (hyperdrive)
  "Return a \"hyper://\"-prefixed URL from a HYPERDRIVE struct.
URL does not have a trailing slash, i.e., \"hyper://PUBLIC-KEY\".

If HYPERDRIVE's public-key slot is empty, use first domain in
domains slot."
  ;; TODO: Add option to prefer domain over public-key
  (pcase-let* (((cl-struct hyperdrive public-key domains) hyperdrive)
               ;; TODO: Fallback to secondary domains?
               (host (or public-key (car domains))))
    (concat "hyper://" host)))

(defun h//url-hexify-string (string)
  "Return STRING having been URL-encoded.
Calls `url-hexify-string' with the \"/\" character added to
`url-unreserved-chars'."
  (url-hexify-string string (cons ?/ url-unreserved-chars)))

(defun he/url (entry)
  "Return ENTRY's canonical URL.
Returns URL with hyperdrive's full public key."
  (h//format-entry-url entry :with-protocol t))

(cl-defun he/create (&key hyperdrive path version etc)
  "Return hyperdrive entry struct from args.
HYPERDRIVE, VERSION, and ETC are used as-is.  Entry NAME is
generated from PATH."
  (setf path (h//format-path path))
  (he//create
   :hyperdrive hyperdrive
   :path path
   ;; TODO: Is it necessary to store the name alongside the path?
   ;;       Instead, only store path and generate name on the fly.
   :name (pcase path
           ("/"
            ;; Root directory: use "/" for clarity.
            "/")
           ((pred (string-suffix-p "/"))
            ;; A subdirectory: keep the trailing slash for clarity
            (file-relative-name path (file-name-parent-directory path)))
           (_
            ;; A file: remove directory part.
            (file-name-nondirectory path)))
   :version version
   :etc etc))

(cl-defun h/sort-entries (entries &key (direction h/directory-sort))
  "Return ENTRIES sorted by DIRECTION.
See `hyperdrive-directory-sort' for the type of DIRECTION."
  (pcase-let* ((`(,column . ,direction) direction)
               ((map (:accessor accessor) (direction sort-function))
                (alist-get column h/dir-sort-fields)))
    (cl-sort entries (lambda (a b)
                       (cond ((and a b) (funcall sort-function a b))
                             ;; When an entry lacks appropriate metadata
                             ;; for sorting by DIRECTION, put it at the end.
                             (a t)))
             :key accessor)))

;;;; API

;; These functions take a URL argument, not a hyperdrive-entry struct.

(cl-defun h/api (method url &rest rest)
  "Make hyperdrive API request by METHOD to URL.
Calls `hyperdrive--httpify-url' to convert HYPER-URL starting
with `hyperdrive--hyper-prefix' to a URL starting with
\"http://localhost:4973/hyper/\" (assuming that
`hyper-gateway-port' is \"4973\").

REST is passed to `plz', which see.

REST may include the argument `:queue', a `plz-queue' in which to
make the request."
  ;; TODO: Document that the request/queue is returned.
  ;; TODO: Should we create a wrapper for `h/api' which calls
  ;;  `h//fill-latest-version' for requests to
  ;;  directories/requests which modify the drive (and therefore
  ;;  always return the latest version number). If we did this, we
  ;;  could remove redundant calls to
  ;;  `h//fill-latest-version' everywhere else.
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
               (else* (apply-partially #'h/api-default-else else)))
    (plist-put rest :else else*)
    (condition-case err
        ;; The `condition-case' is only intended for synchronous
        ;; requests.  Async requests should never signal a `plz-error'
        ;; directly from `plz' or `plz-run'.
        (if-let ((queue (prog1 (plist-get rest :queue)
                          (setf rest (map-delete rest :queue)))))
            (plz-run
             (apply #'plz-queue
                    queue method (h//httpify-url url) rest))
          (apply #'plz method (h//httpify-url url) rest))
      (plz-error
       ;; We pass only the `plz-error' struct to the ELSE* function.
       (funcall else* (caddr err))))))

(defun h/api-default-else (else plz-err)
  "Handle common errors, overriding ELSE.
Checks for common errors; if none are found, calls ELSE with
PLZ-ERR, if ELSE is non-nil; otherwise re-signals PLZ-ERR.
PLZ-ERR should be a `plz-error' struct."
  (pcase plz-err
    ((app plz-error-curl-error `(7 . ,_message))
     ;; Curl error 7 is "Failed to connect to host."
     (h/user-error "Gateway not running.  Use \\[hyperdrive-start] to start it"))
    ((app plz-error-response (cl-struct plz-response (status (or 403 405)) body))
     ;; 403 Forbidden or 405 Method Not Allowed: Display message from hyper-gateway.
     (h/error "%s" body))
    ((guard else)
     (funcall else plz-err))
    (_
     (signal 'plz-error (list "plz error" plz-err)))))

;;;###autoload
(defun hyperdrive-status ()
  "Return non-nil if `hyper-gateway' is running and accessible."
  ;; FIXME: Ensure a very short timeout for this request.
  (condition-case nil
      (plz 'get (concat "http://localhost:" (number-to-string h/hyper-gateway-port) "/"))
    (error nil)))

(defun h//httpify-url (url)
  "Return localhost HTTP URL for HYPER-URL."
  (concat "http://localhost:" (number-to-string h/hyper-gateway-port) "/hyper/"
          (substring url (length h//hyper-prefix))))

(cl-defun h//write (url &key body then else queue)
  "Save BODY (a string) to hyperdrive URL.
THEN and ELSE are passed to `hyperdrive-api', which see."
  (declare (indent defun))
  (h/api 'put url
    ;; TODO: Investigate whether we should use 'text body type for text buffers.
    :body-type 'binary
    ;; TODO: plz accepts buffer as a body, we should refactor calls to h//write to pass in a buffer instead of a buffer-string.
    :body body :as 'response :then then :else else :queue queue))

(defun h/parent (entry)
  "Return parent entry for ENTRY.
If already at top-level directory, return nil."
  (pcase-let (((cl-struct hyperdrive-entry hyperdrive path version) entry))
    (when-let ((parent-path (file-name-parent-directory path)))
      (he/create :hyperdrive hyperdrive :path parent-path :version version))))

;; For Emacsen <29.1.
(declare-function textsec-suspicious-p "ext:textsec-check")
(defun h/url-entry (url)
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
               ;; TODO: For now, no other function besides `h/url-entry' calls
               ;; `h/create', but perhaps it would be good to add a function which wraps
               ;; `h/create' and returns either an existing hyperdrive or a new one?
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
                              (h/create :domains (list host)))
                             (_  ;; Assume host is a public-key
                              (or (gethash host h/hyperdrives)
                                  (h/create :public-key host)))))
               (etc (when target
                      `((target . ,(substring (url-unhex-string target) (length "::"))))))
               (version (pcase path
                          ((rx "/$/version/" (let v (1+ num)) (let p (0+ anything)))
                           (setf path p)
                           (string-to-number v)))))
    ;; e.g. for hyper://PUBLIC-KEY/path/to/basename, we do:
    ;; :path "/path/to/basename" :name "basename"
    (he/create :hyperdrive hyperdrive :path (url-unhex-string path)
               :version version :etc etc)))

;;;; Entries

;; These functions take a hyperdrive-entry struct argument, not a URL.

(defun he/latest (entry)
  "Return ENTRY at its hyperdrive's latest version, or nil."
  (he/at nil entry))

(defun h//entry-version-range-key (entry)
  "Return URI-encoded URL for ENTRY without protocol, version, target, or face.
Intended to be used as hash table key in `hyperdrive-version-ranges'."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path) entry)
               (version-less (he/create :hyperdrive hyperdrive :path path)))
    (substring-no-properties
     (h//format-entry-url version-less :host-format '(public-key)
                          :with-protocol nil :with-target nil))))

;; TODO: Add tests for version range functions
(defun he/version-ranges (entry)
  "Return version ranges for ENTRY."
  (gethash (h//entry-version-range-key entry) h/version-ranges))

(gv-define-setter he/version-ranges (ranges entry)
  `(progn
     (setf (gethash (h//entry-version-range-key ,entry) h/version-ranges) ,ranges)
     (persist-save 'h/version-ranges)))

(defun h/purge-version-ranges (hyperdrive)
  "Purge all version range data for HYPERDRIVE."
  (maphash (lambda (key _val)
             ;; NOTE: The KEY starts with the key and ends with a path, so we compare as prefix.
             (when (string-prefix-p (h/public-key hyperdrive) key)
               (remhash key h/version-ranges)))
           h/version-ranges)
  (persist-save 'h/version-ranges))

(cl-defun he/version-range (entry &key version)
  "Return the version range containing ENTRY.
Returns nil when ENTRY is not known to exist at its version.

With non-nil VERSION, use it instead of ENTRY's version."
  (declare (indent defun))
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive (version entry-version)) entry)
               (version (or version entry-version (h/latest-version hyperdrive)))
               (ranges (he/version-ranges entry)))
    (when ranges
      (cl-find-if (pcase-lambda (`(,range-start . ,(map (:range-end range-end))))
                    (<= range-start version range-end))
                  ranges))))

(cl-defun he/exists-p (entry &key version)
  "Return status of ENTRY's existence at its version.

- t       :: ENTRY is known to exist.
- nil     :: ENTRY is known to not exist.
- unknown :: ENTRY is not known to exist.

Does not make a request to the gateway; checks the cached value
in `hyperdrive-version-ranges'.
With non-nil VERSION, use it instead of ENTRY's version."
  (if-let ((range (he/version-range entry :version version)))
      (pcase-let ((`(,_range-start . ,(map (:existsp existsp))) range))
        existsp)
    'unknown))

(defun he/version-ranges-no-gaps (entry)
  "Return ranges alist for ENTRY with no gaps in history.
Returned newly-constructed alist where each range-end is always
1- the following range-start.  Each gap is filled with a cons cell
whose car is the range start and whose cdr is a plist with a
numerical \\=`:range-end' and \\=`:existsp' set to \\=`unknown':

\(RANGE-START . (\\=`:range-end' RANGE-END \\=`:existsp' \\=`unknown'))

When the final range's range-end is less than ENTRY's
hyperdrive's latest-version slot, the final gap is filled."
  (let ((ranges '())
        (previous-range-end 0))
    (pcase-dolist (`(,range-start . ,(map (:range-end range-end) (:existsp existsp))) (he/version-ranges entry))
      ;; If he/version-ranges returns nil, this whole loop will be skipped.
      (let ((next-range-start (1+ previous-range-end)))
        (when (> range-start next-range-start)
          ;; Insert an "unknown" gap range
          (push `(,next-range-start . (:range-end ,(1- range-start) :existsp unknown)) ranges))
        (push `(,range-start . (:range-end ,range-end :existsp ,existsp)) ranges)
        (setf previous-range-end range-end)))
    (pcase-let* ((final-known-range (car ranges))
                 (`(,_range-start . ,(map (:range-end final-known-range-end))) final-known-range)
                 (latest-version (h/latest-version (he/hyperdrive entry))))
      (unless final-known-range-end
        (setf final-known-range-end 0))
      (when (< final-known-range-end latest-version)
        ;; Insert possible final gap between latest known range and hyperdrive's latest-version
        (push `(,(1+ final-known-range-end) . (:range-end ,latest-version , :existsp unknown)) ranges)))
    (nreverse ranges)))

(cl-defun he/previous (entry &key cache-only)
  "Return ENTRY at its hyperdrive's previous version, or nil.
If ENTRY is a directory, return a copy with decremented version.
If CACHE-ONLY, don't send a request to the gateway; only check
`hyperdrive-version-ranges'.  In this case, return value may also
be \\+`unknown'."
  (if (h//entry-directory-p entry)
      (pcase-let* (((cl-struct hyperdrive-entry hyperdrive path version) entry)
                   (version (or version (h/latest-version hyperdrive))))
        (when (> version 1)
          (he/create :hyperdrive hyperdrive :path path :version (1- version))))
    (let ((previous-version (1- (car (he/version-range entry)))))
      (pcase-exhaustive (he/version-range entry :version previous-version)
        (`(,range-start . ,(map (:existsp existsp)))
         (if existsp
             ;; Return entry if it's known existent.
             (he/at range-start entry)
           ;; Return nil if it's known nonexistent.
           nil))
        ('nil
         ;; Entry is not known to exist, optionally send a request.
         (if cache-only
             'unknown
           (when-let ((previous-entry (he/at previous-version entry)))
             ;; Entry version is currently its range end, but it should be its version range start.
             (setf (he/version previous-entry) (car (he/version-range previous-entry)))
             previous-entry)))))))

(defun he/at (version entry)
  "Return ENTRY at its hyperdrive's VERSION, or nil if not found.
When VERSION is nil, return latest version of ENTRY."
  ;; Use `h/copy-tree', because `copy-tree' doesn't work on
  ;; records/structs, and `copy-hyperdrive-entry' doesn't copy deeply,
  ;; and we need to be able to modify the `etc' alist of the copied
  ;; entry separately.
  (let ((entry (h/copy-tree entry t)))
    (setf (he/version entry) version)
    (condition-case err
        ;; FIXME: Requests to out of range version currently hang.
        (h/fill entry :then 'sync)
      (plz-error
       (pcase (plz-response-status (plz-error-response (caddr err)))
         ;; FIXME: If plz-error is a curl-error, this block will fail.
         (404 nil)
         (_ (signal (car err) (cdr err))))))))

(cl-defun he/next (entry)
  "Return unfilled ENTRY at its hyperdrive's next version.

If next version is known nonexistent, return nil.
If next version's existence is unknown, return \\+`unknown'.
If ENTRY's version is nil, return value is `eq' to ENTRY.

Sends a request to the gateway for hyperdrive's latest version."
  (unless (he/version entry)
    ;; ENTRY's version is nil: return ENTRY.
    (cl-return-from he/next entry))

  ;; ENTRY's version is not nil.
  (let ((next-entry (h/copy-tree entry t))
        (latest-version (h/fill-latest-version
                         (he/hyperdrive entry))))

    ;; ENTRY version is the latest version: return ENTRY with nil version.
    (when (eq latest-version (he/version entry))
      (setf (he/version next-entry) nil)
      (cl-return-from he/next next-entry))

    ;; ENTRY is a directory: increment the version number by one.
    (when (h//entry-directory-p entry)
      (cl-incf (he/version next-entry))
      (when (eq latest-version (he/version next-entry))
        ;; Next ENTRY is the latest version: return ENTRY with nil version.
        (setf (he/version next-entry) nil))
      (cl-return-from he/next next-entry))

    ;; ENTRY is a file...
    (pcase-let* ((`(,_range-start . ,(map (:range-end range-end))) (he/version-range entry))
                 (next-range-start (1+ range-end))
                 ((map (:existsp next-range-existsp) (:range-end next-range-end))
                  ;; TODO: If cl struct copiers are extended like this:
                  ;;       https://lists.gnu.org/archive/html/help-gnu-emacs/2021-10/msg00797.html
                  ;;       replace following sexp with
                  ;;       (he/version-range (hyperdrive-entry-copy :version next-range-start))
                  (map-elt (he/version-ranges-no-gaps entry) next-range-start)))
      ;; ENTRY is in the last version range: return ENTRY with nil version.
      (when (eq latest-version range-end)
        (setf (he/version next-entry) nil)
        (cl-return-from he/next next-entry))

      ;; Check existence of ENTRY's next version range...
      (pcase-exhaustive next-range-existsp
        ('t
         (setf (he/version next-entry)
               (if (eq next-range-end latest-version)
                   ;; This is the latest version: remove version number.
                   nil
                 next-range-start))
         next-entry)
        ('nil nil)
        ('unknown 'unknown)))))

(declare-function h/history "hyperdrive-history")
(cl-defun h/open
    (entry &key recurse (createp t) (messagep t)
           (then (lambda ()
                   (pop-to-buffer (current-buffer) '((display-buffer-reuse-window display-buffer-same-window))))))
  "Open hyperdrive ENTRY.
If RECURSE, proceed up the directory hierarchy if given path is
not found.  THEN is a function to pass to the handler which will
be called with no arguments in the buffer opened by the handler.
When a writable ENTRY is not found and CREATEP is non-nil, create
a new buffer for ENTRY.  When MESSAGEP, show a message in the
echo area when the request for the file is made."
  (declare (indent defun))
  ;; TODO: Add `find-file'-like interface. See <https://todo.sr.ht/~ushin/ushin/16>
  ;; FIXME: Some of the synchronous filling functions we've added now cause this to be blocking,
  ;; which is very noticeable when a file can't be loaded from the gateway and eventually times out.
  (let ((hyperdrive (he/hyperdrive entry)))
    (h/fill entry
      :then (lambda (entry)
              (pcase-let* (((cl-struct hyperdrive-entry type) entry)
                           (handler (alist-get type h/type-handlers nil nil #'string-match-p)))
                (unless (h//entry-directory-p entry)
                  ;; No need to fill latest version for directories,
                  ;; since we do it in `h//fill' already.
                  (h/fill-latest-version hyperdrive))
                (h/persist hyperdrive)
                (funcall (or handler #'h/handler-default) entry :then then)))
      :else (lambda (err)
              (cl-labels ((not-found-action
                            () (if recurse
                                   (h/open (h/parent entry) :recurse t)
                                 (pcase (prompt)
                                   ('history (h/history entry))
                                   ('up (h/open (h/parent entry)))
                                   ('recurse (h/open (h/parent entry) :recurse t)))))
                          (prompt
                            () (pcase-exhaustive
                                   (read-answer (format "URL not found: \"%s\". " (he/url entry))
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
                    ((equal (he/path entry) "/")
                     ;; Root directory not found: Drive has not been
                     ;; loaded locally, and no peers are found seeding it.
                     (h/message "No peers found for %s" (he/url entry)))
                    ((and createp
                          (not (h//entry-directory-p entry))
                          (h/writablep hyperdrive)
                          (not (he/version entry)))
                     ;; Entry is a writable file: create a new buffer
                     ;; that will be saved to its path.
                     (if-let ((buffer
                               (get-buffer
                                (h//format-entry entry h/buffer-name-format))))
                         ;; Buffer already exists: likely the user deleted the entry
                         ;; without killing the buffer.  Switch to the buffer and
                         ;; alert the user that the entry no longer exists.
                         (progn
                           (switch-to-buffer buffer)
                           (h/message "Entry no longer exists!  %s"
                                      (h//format-entry entry)))
                       ;; Make and switch to new buffer.
                       (switch-to-buffer (h//get-buffer-create entry))))
                    (t
                     ;; Hyperdrive entry is not writable: prompt for action.
                     (not-found-action))))
                  (500 ;; Generic error, likely a mistyped URL
                   (h/message "Generic hyper-gateway status 500 error. Is this URL correct? %s"
                              (he/url entry)))
                  (_ (h/message "Unable to load URL \"%s\": %S"
                                (he/url entry) err))))))
    (when messagep
      (h/message "Opening <%s>..." (he/url entry)))))

(cl-defun h/fill (entry &key queue then else)
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
                    (h/message
                     (format "hyperdrive-fill: error: %S" plz-error)))))))
  (pcase then
    ('sync (condition-case err
               (h//fill entry
                        (plz-response-headers
                         (h/api 'head (he/url entry)
                           :as 'response
                           :then 'sync
                           :noquery t)))
             (plz-error
              (pcase (plz-response-status (plz-error-response (caddr err)))
                ;; FIXME: If plz-error is a curl-error, this block will fail.
                (404 ;; Entry doesn't exist at this version: update range data.
                 (h/update-nonexistent-version-range entry)))
              ;; Re-signal error for, e.g. `he/at'.
              (signal (car err) (cdr err)))))
    (_ (h/api 'head (he/url entry)
         :queue queue
         :as 'response
         :then (lambda (response)
                 (funcall then (h//fill entry (plz-response-headers response))))
         :else (lambda (&rest args)
                 (when (he/version entry)
                   ;; If request is canceled, the entry may not have a version.
                   ;; FIXME: Only update nonexistent range on 404.
                   (h/update-nonexistent-version-range entry))
                 (apply else args))
         :noquery t))))

(defun h//fill (entry headers)
  "Fill ENTRY and its hyperdrive from HEADERS.

The following ENTRY slots are filled:
- \\+`type'
- \\+`mtime'
- \\+`size'
- \\+`hyperdrive' (from persisted value if it exists)

The following ENTRY hyperdrive slots are filled:
- \\+`public-key'
- \\+`writablep' (when headers include Allow)
- \\+`domains' (merged with current persisted value)

Returns filled ENTRY."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive) entry)
               ((cl-struct hyperdrive writablep domains) hyperdrive)
               ((map link content-length content-type etag last-modified allow) headers)
               ;; If URL hostname was a DNSLink domain, entry doesn't yet have a public-key slot.
               (public-key (progn
                             (string-match h//public-key-re link)
                             (match-string 1 link)))
               (persisted-hyperdrive (gethash public-key h/hyperdrives))
               (domain (car domains)))
    (when last-modified
      (setf last-modified (encode-time (parse-time-string last-modified))))
    (when (and allow (eq 'unknown writablep))
      (setf (h/writablep hyperdrive) (string-match-p "PUT" allow)))
    (setf (he/size entry) (when content-length
                            (ignore-errors
                              (cl-parse-integer content-length)))
          (he/type entry) content-type
          (he/mtime entry) last-modified)
    (if persisted-hyperdrive
        (progn
          ;; Ensure that entry's hyperdrive is the persisted
          ;; hyperdrive, since it may be used later as part of a
          ;; `h/version-ranges' key and compared using `eq'.
          ;; Also, we want the call to `h//fill-latest-version'
          ;; below to update the persisted hyperdrive.
          (setf (he/hyperdrive entry) persisted-hyperdrive)
          (when domain
            ;; The previous call to he/url may not have retrieved
            ;; the persisted hyperdrive if we had only a domain but no public-key.
            (cl-pushnew domain (h/domains (he/hyperdrive entry)) :test #'equal)))
      (setf (h/public-key hyperdrive) public-key))
    (if (and (h//entry-directory-p entry)
             (null (he/version entry)))
        ;; Version-less directory HEAD/GET request ETag header always have the
        ;; hyperdrive's latest version. We don't currently store
        ;; version ranges for directories (since they don't
        ;; technically have versions in hyperdrive).
        (h//fill-latest-version hyperdrive headers)
      ;; File HEAD/GET request ETag header does not retrieve the
      ;; hyperdrive's latest version, so `h/update-existent-version-range'
      ;; will not necessarily fill in the entry's last range.
      (h/update-existent-version-range entry (string-to-number etag)))
    entry))

(defun h/fill-latest-version (hyperdrive)
  "Synchronously fill the latest version slot in HYPERDRIVE.
Returns the latest version number."
  (pcase-let (((cl-struct plz-response headers)
               (h/api
                 'head (he/url
                        (he/create
                         :hyperdrive hyperdrive :path "/"))
                 :as 'response)))
    (h//fill-latest-version hyperdrive headers)))

(defun h//fill-latest-version (hyperdrive headers)
  "Fill the latest version slot in HYPERDRIVE from HEADERS.
HEADERS must from a HEAD/GET request to a directory or a
PUT/DELETE request to a file, as only those requests return the
correct ETag header.  Returns the latest version number."
  ;; TODO: Update relevant buffers when hyperdrive latest version
  ;; updates, at the least describe-hyperdrive buffers.
  ;; TODO: Consider updating version range here. First check all the
  ;; places where this function is called. Better yet, update
  ;; `h/version-ranges' (and `h/hyperdrives'?) in a
  ;; lower-level function, perhaps a wrapper for `h/api'?
  (setf (h/latest-version hyperdrive) (string-to-number (map-elt headers 'etag))))

;; TODO: Consider using symbol-macrolet to simplify place access.

(defun h/update-existent-version-range (entry range-start)
  "Update the version range for ENTRY which exists at its version.
Sets the range keyed by RANGE-START to a plist whose :range-end
value is ENTRY's version.

For the format of each version range, see `hyperdrive-version-ranges'.

Returns the ranges cons cell for ENTRY."
  (cl-check-type range-start integer)
  (unless (h//entry-directory-p entry)
    (pcase-let* ((ranges (he/version-ranges entry))
                 (range (map-elt ranges range-start))
                 ((map (:range-end old-range-end)) range)
                 ((cl-struct hyperdrive-entry hyperdrive version) entry)
                 (range-end (or version (h/latest-version hyperdrive))))
      (unless (and old-range-end (> old-range-end range-end))
        ;; If there already exists a longer existent range in
        ;; `h/version-ranges', there's nothing to do.
        (setf (plist-get range :existsp) t
              (plist-get range :range-end) range-end
              (map-elt ranges range-start) range
              (he/version-ranges entry) (cl-sort ranges #'< :key #'car))))))

(defun h/update-nonexistent-version-range (entry)
  "Update the version range for ENTRY which doesn't exist at its version.
Checks for nonexistent previous or next ranges, to combine them
into one contiguous nonexistent range.

For the format of each version range, see `hyperdrive-version-ranges'.

Returns the ranges cons cell for ENTRY."
  (unless (or (h//entry-directory-p entry)
              ;; If there already exists a nonexistent range in
              ;; `h/version-ranges', there's nothing to do.
              (he/version-range entry)
              ;; Don't store ranges for entries which have never existed.
              (not (he/version-ranges entry)))
    (pcase-let* ((ranges (he/version-ranges entry))
                 ((cl-struct hyperdrive-entry hyperdrive path version) entry)
                 (version (or version (h/latest-version hyperdrive)))
                 (previous-range (he/version-range
                                   (he/create :hyperdrive hyperdrive :path path :version (1- version))))
                 (`(,previous-range-start . ,(map (:existsp previous-exists-p))) previous-range)
                 (next-range (he/version-range
                               (he/create :hyperdrive hyperdrive :path path :version (1+ version))))
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
            (he/version-ranges entry) (cl-sort ranges #'< :key #'car)))))

(cl-defun h/fill-version-ranges (entry &key (finally #'ignore))
  "Asynchronously fill in versions ranges before ENTRY.
Once all requests return, call FINALLY with no arguments."
  (declare (indent defun))
  (let* ((outstanding-nonexistent-requests-p)
         (total-requests-limit h/fill-version-ranges-limit)
         (fill-entry-queue (make-plz-queue :limit h/queue-limit
                                           :finally (lambda ()
                                                      (unless outstanding-nonexistent-requests-p
                                                        (funcall finally)))))
         ;; Flag used in the nonexistent-queue finalizer.
         finishedp)
    (cl-labels ((fill-existent-at (version)
                  (let ((prev-range-end (1- (car (he/version-range entry :version version)))))
                    (if (and (cl-plusp total-requests-limit)
                             (eq 'unknown (he/exists-p entry :version prev-range-end)))
                        ;; Recurse backward through history.
                        (fill-entry-at prev-range-end)
                      (setf finishedp t))))
                (fill-nonexistent-at (version)
                  (let ((nonexistent-queue
                         (make-plz-queue
                          :limit h/queue-limit
                          :finally (lambda ()
                                     (setf outstanding-nonexistent-requests-p nil)
                                     (if finishedp
                                         ;; If the fill-nonexistent-at loop stopped
                                         ;; prematurely, stop filling and call `finally'.
                                         (funcall finally)
                                       (let ((last-requested-version (- version h/queue-limit)))
                                         (cl-decf total-requests-limit h/queue-limit)
                                         (pcase-exhaustive (he/exists-p entry :version last-requested-version)
                                           ('t (fill-existent-at last-requested-version))
                                           ('nil (fill-nonexistent-at last-requested-version))
                                           ('unknown
                                            (h/error "Entry should have been filled at version: %s" last-requested-version))))))))
                        ;; Make a copy of the version ranges for use in the HEAD request callback.
                        (copy-entry-version-ranges (copy-sequence (he/version-ranges entry))))
                    ;; For nonexistent entries, send requests in parallel.
                    (cl-dotimes (i h/queue-limit)
                      ;; Send the maximum number of simultaneous requests.
                      (let ((prev-entry (h/copy-tree entry t)))
                        (setf (he/version prev-entry) (- version i 1))
                        (unless (and (cl-plusp (he/version prev-entry))
                                     (eq 'unknown (he/exists-p prev-entry))
                                     (> total-requests-limit i))
                          ;; Stop at the beginning of the history, at a known
                          ;; existent/nonexistent entry, or at the limit.
                          (setf finishedp t)
                          (cl-return))
                        (h/api 'head (he/url prev-entry)
                          :queue nonexistent-queue
                          :as 'response
                          :then (pcase-lambda ((cl-struct plz-response (headers (map etag))))
                                  (pcase-let* ((range-start (string-to-number etag))
                                               ((map (:existsp existsp)) (map-elt copy-entry-version-ranges range-start)))
                                    (when (eq 'unknown existsp)
                                      ;; Stop if the requested entry has a
                                      ;; range-start that was already known
                                      ;; before this batch of parallel requests.
                                      (setf finishedp t))
                                    (h/update-existent-version-range prev-entry range-start)))
                          :else (lambda (err)
                                  ;; TODO: Better error handling.
                                  (pcase (plz-response-status (plz-error-response err))
                                    ;; FIXME: If plz-error is a curl-error, this block will fail.
                                    (404 (h/update-nonexistent-version-range prev-entry))
                                    (_ (signal (car err) (cdr err)))))
                          :noquery t)
                        (setf outstanding-nonexistent-requests-p t)))))
                (fill-entry-at (version)
                  (let ((copy-entry (h/copy-tree entry t)))
                    (setf (he/version copy-entry) version)
                    (cl-decf total-requests-limit)
                    (h/api 'head (he/url copy-entry)
                      :queue fill-entry-queue
                      :as 'response
                      :then (pcase-lambda ((cl-struct plz-response (headers (map etag))))
                              (pcase-let* ((range-start (string-to-number etag))
                                           ((map (:existsp existsp))
                                            (map-elt (he/version-ranges copy-entry) range-start)))
                                (h/update-existent-version-range copy-entry range-start)
                                (if (eq 't existsp)
                                    ;; Stop if the requested entry has a
                                    ;; range-start that was already known
                                    ;; before this batch of parallel requests.
                                    (setf finishedp t)
                                  (fill-existent-at version))))
                      :else (lambda (err)
                              (pcase (plz-response-status (plz-error-response err))
                                ;; FIXME: If plz-error is a curl-error, this block will fail.
                                (404
                                 (h/update-nonexistent-version-range copy-entry)
                                 (fill-nonexistent-at version))
                                (_ (signal (car err) (cdr err)))))
                      :noquery t))))
      (fill-entry-at (he/version entry)))))

(defun h/fill-metadata (hyperdrive)
  "Fill HYPERDRIVE's public metadata and return it.
Sends a synchronous request to get the latest contents of
HYPERDRIVE's public metadata file."
  (declare (indent defun))
  (pcase-let* ((entry (he/create
                       :hyperdrive hyperdrive
                       :path "/.well-known/host-meta.json"
                       ;; NOTE: Don't attempt to fill hyperdrive struct with old metadata
                       :version nil))
               (metadata (condition-case err
                             (h/api 'get (he/url entry)
                               :as (lambda ()
                                     (condition-case err
                                         (json-read)
                                       (json-error
                                        (h/message "Error parsing JSON metadata file: %s"
                                                   (he/url entry)))
                                       (_ (signal (car err) (cdr err)))))
                               :noquery t)
                           (plz-error
                            (pcase (plz-response-status (plz-error-response (caddr err)))
                              ;; FIXME: If plz-error is a curl-error, this block will fail.
                              (404 nil)
                              (_ (signal (car err) (cdr err))))))))
    (setf (h/metadata hyperdrive) metadata)
    (h/persist hyperdrive)
    hyperdrive))

(cl-defun h/purge-no-prompt (hyperdrive &key then else)
  "Purge all data corresponding to HYPERDRIVE, then call THEN with response.

- HYPERDRIVE file content and metadata managed by hyper-gateway
- hash table entry for HYPERDRIVE in `hyperdrive-hyperdrives'
- hash table entries for HYPERDRIVE in `hyperdrive-version-ranges'

Call ELSE if request fails."
  (declare (indent defun))
  (h/api 'delete (he/url (he/create :hyperdrive hyperdrive))
    :as 'response
    :then (lambda (response)
            (h/persist hyperdrive :purge t)
            (h/purge-version-ranges hyperdrive)
            (funcall then response))
    :else else))

(cl-defun h/write (entry &key body then else queue)
  "Write BODY to hyperdrive ENTRY's URL."
  (declare (indent defun))
  (h//write (he/url entry)
    :body body :then then :else else :queue queue))

(cl-defun h//format-entry-url
    (entry &key (host-format '(public-key domain))
           (with-path t) (with-protocol t) (with-help-echo t) (with-target t))
  "Return ENTRY's URL.
Returns URL formatted like:

  hyper://HOST-FORMAT/PATH/TO/FILE

HOST-FORMAT is passed to `hyperdrive--preferred-format', which see.
If WITH-PROTOCOL, \"hyper://\" is prepended.  If WITH-HELP-ECHO,
propertize string with `help-echo' property showing the entry's
full URL.  If WITH-TARGET, append the ENTRY's target, stored in
its :etc slot.  If WITH-PATH, include the path portion.  When
ENTRY has non-nil `version' slot, include version number in URL.

Note that, if HOST-FORMAT includes values other than `public-key'
and `domain', the resulting URL may not be a valid hyperdrive
URL.

Path and target fragment are URI-encoded."
  ;; NOTE: Entries may have only a domain, not a public key yet, so we
  ;; include `domain' in HOST-FORMAT's default value.  The public key
  ;; will be filled in later.
  (pcase-let* (((cl-struct hyperdrive-entry path version etc)
                entry)
               (protocol (when with-protocol
                           "hyper://"))
               (host (when host-format
                       ;; FIXME: Update docstring to say that host-format can be nil to omit it.
                       (h//preferred-format (he/hyperdrive entry)
                                            host-format h/raw-formats)))
               (version-part (and version (format "/$/version/%s" version)))
               ((map target) etc)
               (target-part (when (and with-target target)
                              (concat "#" (url-hexify-string "::")
                                      (url-hexify-string target))))
               (path (when with-path
                       ;; TODO: Consider removing this argument if it's not needed.
                       (h//url-hexify-string path)))
               (url (concat protocol host version-part path target-part)))
    (if with-help-echo
        (propertize url
                    'help-echo (h//format-entry-url
                                entry :with-protocol t :host-format '(public-key domain)
                                :with-path with-path :with-help-echo nil :with-target with-target))
      url)))

(defun h//format (hyperdrive &optional format formats)
  "Return HYPERDRIVE formatted according to FORMAT.
FORMAT is a `format-spec' specifier string which maps to specifications
according to FORMATS, by default `hyperdrive-formats', which see."
  (pcase-let* (((cl-struct hyperdrive domains public-key petname seed
                           (metadata (map ('name nickname))))
                hyperdrive)
               (format (or format "%H"))
               (formats (or formats h/formats)))
    (cl-labels ((fmt (format value face)
                  (if value
                      (format (alist-get format formats)
                              (propertize value 'face face))
                    "")))
      (format-spec format
                   ;; TODO(deprecate-28): Use lambdas in each specifier.
                   `((?H . ,(and (string-match-p (rx "%"
                                                     ;; Flags
                                                     (optional (1+ (or " " "0" "-" "<" ">" "^" "_")))
                                                     (0+ digit) ;; Width
                                                     (0+ digit) ;; Precision
                                                     "H")
                                                 format)
                                 ;; HACK: Once using lambdas in this specifier,
                                 ;; remove the `string-match-p' check.
                                 (h//preferred-format hyperdrive)))
                     (?P . ,(fmt 'petname petname 'h/petname))
                     (?N . ,(fmt 'nickname nickname 'h/nickname))
                     (?k . ,(fmt 'short-key public-key 'h/public-key))
                     (?K . ,(fmt 'public-key public-key 'h/public-key))
                     (?S . ,(fmt 'seed seed 'h/seed))
                     (?D . ,(if (car domains)
                                (format (alist-get 'domains formats)
                                        (string-join
                                         (mapcar (lambda (domain)
                                                   (propertize domain
                                                               'face 'h/domain))
                                                 domains)
                                         ","))
                              "")))))))

(defun h//preferred-format (hyperdrive &optional format formats)
  "Return HYPERDRIVE's formatted hostname, or nil.
FORMAT should be one or a list of symbols, by default
`hyperdrive-preferred-formats', which see for choices.  If the
specified FORMAT is not available, return nil.

Each item in FORMAT is formatted according to FORMATS, set by
default to `hyperdrive-formats', which see."
  (pcase-let* (((cl-struct hyperdrive petname public-key domains seed
                           (metadata (map ('name nickname))))
                hyperdrive))
    (cl-loop for f in (ensure-list (or format h/preferred-formats))
             when (pcase f
                    ((and 'petname (guard petname))
                     (h//format hyperdrive "%P" formats))
                    ((and 'nickname (guard nickname))
                     (h//format hyperdrive "%N" formats))
                    ((and 'domain (guard (car domains)))
                     (h//format hyperdrive "%D" formats))
                    ((and 'seed (guard seed))
                     (h//format hyperdrive "%S" formats))
                    ((and 'short-key (guard public-key))
                     (h//format hyperdrive "%k" formats))
                    ((and 'public-key (guard public-key))
                     (h//format hyperdrive "%K" formats)))
             return it)))

;;;; Reading from the user

(declare-function h/dir--entry-at-point "hyperdrive-dir")
(cl-defun h//context-entry (&key latest-version)
  "Return the current entry in the current context.
LATEST-VERSION is passed to `hyperdrive-read-entry'.
With universal prefix argument \\[universal-argument], prompt for entry."
  (pcase major-mode
    ((guard current-prefix-arg)
     (h/read-entry :read-version t :latest-version latest-version))
    ('h/dir-mode (h/dir--entry-at-point))
    (_ (or h/current-entry (h/read-entry :latest-version latest-version)))))

(cl-defun h/complete-hyperdrive (&key predicate force-prompt)
  "Return hyperdrive for current entry when it matches PREDICATE.

With FORCE-PROMPT or when current hyperdrive does not match
PREDICATE, return a hyperdrive selected with completion.  In this
case, when PREDICATE, only offer hyperdrives matching it."
  (when (zerop (hash-table-count h/hyperdrives))
    (h/user-error "No known hyperdrives.  Use `hyperdrive-new' to create a new one"))
  (unless predicate
    ;; cl-defun default value doesn't work when nil predicate value is passed in.
    (setf predicate #'always))

  ;; Return current drive when appropriate.
  (when-let* (((not force-prompt))
              (h/current-entry)
              (current-hyperdrive (he/hyperdrive h/current-entry))
              ((funcall predicate current-hyperdrive)))
    (cl-return-from h/complete-hyperdrive current-hyperdrive))

  ;; Otherwise, prompt for drive.
  (let* ((current-hyperdrive (when h/current-entry
                               (he/hyperdrive h/current-entry)))
         (hyperdrives (cl-remove-if-not predicate (hash-table-values h/hyperdrives)))
         (default (when (and h/current-entry (funcall predicate current-hyperdrive))
                    (h//format-hyperdrive (he/hyperdrive h/current-entry))))
         (prompt (format-prompt "Hyperdrive" default))
         (candidates (mapcar (lambda (hyperdrive)
                               (cons (h//format-hyperdrive hyperdrive) hyperdrive))
                             hyperdrives))
         (completion-styles (cons 'substring completion-styles))
         (selected
          (completing-read
           prompt
           (lambda (string predicate action)
             (if (eq action 'metadata)
                 '(metadata (category . hyperdrive))
               (complete-with-action
                action candidates string predicate)))
           nil 'require-match nil nil default)))
    (or (alist-get selected candidates nil nil #'equal)
        (h/user-error "No such hyperdrive.  Use `hyperdrive-new' to create a new one"))))

(cl-defun h//format-hyperdrive
    (hyperdrive &key (formats '(petname nickname domain seed short-key)))
  "Return HYPERDRIVE formatted for completion.
For each of FORMATS, concatenates the value separated by two spaces."
  (string-trim
   (cl-loop for format in formats
            when (h//preferred-format hyperdrive format)
            concat (concat it "  "))))

(cl-defun h/read-entry (&key hyperdrive predicate default-path
                             (force-prompt-drive t) latest-version read-version)
  "Return new hyperdrive entry in HYPERDRIVE with path read from user.

With nil HYPERDRIVE, prompt for one by passing PREDICATE and
FORCE-PROMPT-DRIVE to `hyperdrive-complete-hyperdrive'.

If DEFAULT-PATH, offer it as the default entry path.  Otherwise,
offer the path of `hyperdrive-current-entry' when it is in the
hyperdrive chosen with completion.

When LATEST-VERSION is non-nil, returned entry's version is nil.
When LATEST-VERSION is nil, READ-VERSION is non-nil, and
`hyperdrive-current-entry' is in the hyperdrive chosen with
completion, returned entry has the same version.
Otherwise, prompt for a version number."
  ;; TODO: Consider removing FORCE-PROMPT-DRIVE argument.
  (let* ((hyperdrive (or hyperdrive
                         (h/complete-hyperdrive :predicate predicate
                                                :force-prompt force-prompt-drive)))
         (default-version (when (and (not latest-version)
                                     h/current-entry
                                     (h/equal-p
                                      hyperdrive (he/hyperdrive h/current-entry)))
                            (he/version h/current-entry)))
         (version (unless latest-version
                    (if read-version
                        (h/read-version :hyperdrive hyperdrive :initial-input-number default-version)
                      default-version)))
         (default-path (h//format-path
                        (or default-path
                            (and h/current-entry
                                 (h/equal-p
                                  hyperdrive (he/hyperdrive h/current-entry))
                                 (he/path h/current-entry)))))
         (path (h/read-path :hyperdrive hyperdrive :version version :default default-path)))
    (he/create :hyperdrive hyperdrive :path path :version version)))

(defvar h//version-history nil
  "Minibuffer history of `hyperdrive-read-version'.")

(cl-defun h/read-version (&key hyperdrive prompt initial-input-number)
  "Return version number.
Blank input returns nil.

HYPERDRIVE is used to fill in PROMPT format %s sequence.
INITIAL-INPUT-NUMBER is converted to a string and passed to
`read-string', which see."
  (let* ((prompt (or prompt "Version number in `%s' (leave blank for latest version)"))
         ;; Don't use read-number since it cannot return nil.
         (version (read-string
                   (format-prompt prompt nil (h//format-hyperdrive hyperdrive))
                   (when initial-input-number (number-to-string initial-input-number))
                   'h//version-history)))
    (unless (string-blank-p version)
      (string-to-number version))))

(defvar h//path-history nil
  "Minibuffer history of `hyperdrive-read-path'.")

(cl-defun h/read-path (&key hyperdrive version prompt default)
  "Return path read from user.
HYPERDRIVE and VERSION are used to fill in the prompt's format %s
sequence.  PROMPT is passed to `format-prompt', which see.  DEFAULT
is passed to `read-string' as its DEFAULT-VALUE argument."
  (let ((prompt (or prompt
                    (if version
                        "Path in `%s' (version:%s)"
                      "Path in `%s'"))))
    ;; TODO: Provide a `find-file'-like auto-completing UI
    (read-string (format-prompt prompt default
                                (h//format-hyperdrive hyperdrive) version)
                 nil 'h//path-history default)))

(defvar h//url-history nil
  "Minibuffer history of `hyperdrive-read-url'.")

(cl-defun h/read-url (&key (prompt "Hyperdrive URL"))
  "Return URL trimmed of whitespace.
Prompts with PROMPT.  Defaults to current entry if it exists."
  (let ((default (when h/current-entry
                   (he/url h/current-entry))))
    (string-trim (read-string (format-prompt prompt default) nil 'h//url-history default))))

(defvar h//name-history nil
  "Minibuffer history of `hyperdrive-read-name'.")

(cl-defun h/read-name (&key prompt initial-input default)
  "Wrapper for `read-string' with common history.
Prompts with PROMPT and DEFAULT, according to `format-prompt'.
DEFAULT and INITIAL-INPUT are passed to `read-string' as-is."
  (read-string (format-prompt prompt default) initial-input 'h//name-history default))

(cl-defun h/put-metadata (hyperdrive &key then)
  "Put HYPERDRIVE's metadata into the appropriate file, then call THEN."
  (declare (indent defun))
  (let ((entry (he/create :hyperdrive hyperdrive
                          :path "/.well-known/host-meta.json")))
    (h/write entry :body (json-encode (h/metadata hyperdrive))
      :then then)
    hyperdrive))

(cl-defun h/persist (hyperdrive &key purge)
  "Persist HYPERDRIVE in `hyperdrive-hyperdrives'.
With PURGE, delete hash table entry for HYPERDRIVE."
  ;; TODO: Make separate function for purging persisted data.
  (if purge
      (remhash (h/public-key hyperdrive) h/hyperdrives)
    (puthash (h/public-key hyperdrive) hyperdrive h/hyperdrives))
  (persist-save 'h/hyperdrives))

(defun h/seed-url (seed)
  "Return URL to hyperdrive known as SEED, or nil if it doesn't exist.
That is, if the SEED has been used to create a local
hyperdrive."
  (condition-case err
      (pcase (h/api 'get (concat "hyper://localhost/?key=" (url-hexify-string seed))
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
                             ('seed #'h/seed)
                             ('petname #'h/petname)
                             ('public-key #'h/public-key))))
    (catch 'get-first-hash
      (maphash (lambda (_key val)
                 (when (equal (funcall accessor-function val) value)
                   (throw 'get-first-hash val)))
               h/hyperdrives)
      nil)))

;;;; Handlers

(declare-function h/org--link-goto "hyperdrive-org")
(cl-defun h/handler-default (entry &key then)
  "Load ENTRY's file into an Emacs buffer.
If then, then call THEN with no arguments.  Default handler."
  (h/api 'get (he/url entry)
    :noquery t
    :as (lambda ()
          (pcase-let* (((cl-struct hyperdrive-entry hyperdrive version etc) entry)
                       ((map target) etc)
                       (response-buffer (current-buffer)))
            (with-current-buffer (h//get-buffer-create entry)
              ;; TODO: Don't reload if we're jumping to a link on the
              ;; same page (but ensure that reverting still works).
              (if (buffer-modified-p)
                  (h/message "Buffer modified: %S" (current-buffer))
                (save-excursion
                  (with-silent-modifications
                    (erase-buffer)
                    (insert-buffer-substring response-buffer))
                  (setf buffer-undo-list nil
                        buffer-read-only (or (not (h/writablep hyperdrive)) version))
                  (set-buffer-modified-p nil)
                  (set-visited-file-modtime (current-time))))
              (when target
                (pcase major-mode
                  ('org-mode
                   (require 'hyperdrive-org)
                   (h/org--link-goto target))
                  ('markdown-mode
                   ;; TODO: Handle markdown link
                   )))
              (when then
                (funcall then)))))))

(cl-defun h/handler-streamable (entry &key _then)
  ;; TODO: Is there any reason to not pass THEN through?
  "Stream ENTRY."
  (h/message "Streaming %s..." (h//format-entry-url entry))
  (pcase-let ((`(,command . ,args)
               (split-string h/stream-player-command)))
    (apply #'start-process "hyperdrive-stream-player"
           nil command (cl-substitute (h//httpify-url
                                       (he/url entry))
                                      "%s" args :test #'equal))))

(declare-function h/dir-handler "hyperdrive-dir")
(cl-defun h/handler-json (entry &key then)
  "Show ENTRY.
THEN is passed to other handlers, which see.  If ENTRY is a
directory (if its URL ends in \"/\"), pass to
`hyperdrive-dir-handler'.  Otherwise, open with
`hyperdrive-handler-default'."
  (if (h//entry-directory-p entry)
      (h/dir-handler entry :then then)
    (h/handler-default entry :then then)))

(cl-defun h/handler-html (entry &key then)
  "Show ENTRY, where ENTRY is an HTML file.
If `hyperdrive-render-html' is non-nil, render HTML with
`shr-insert-document', then calls THEN if given.  Otherwise, open
with `hyperdrive-handler-default'."
  (if h/render-html
      (let (buffer)
        (save-window-excursion
          ;; Override EWW's calling `pop-to-buffer-same-window'; we
          ;; want our callback to display the buffer.
          (eww (he/url entry))
          ;; Set `h/current-entry' and use `h/mode'
          ;; for remapped keybindings for, e.g., `h/up'.
          (setq-local h/current-entry entry)
          (h/mode)
          (setq buffer (current-buffer)))
        (set-buffer buffer)
        (when then
          (funcall then)))
    (h/handler-default entry :then then)))

(cl-defun h/handler-image (entry &key then)
  "Show ENTRY, where ENTRY is an image file.
Then calls THEN if given."
  (h/handler-default
   entry :then (lambda ()
		 (image-mode)
		 (when then
		   (funcall then)))))

;;;; Gateway process

;; NOTE: The below involves some slightly hacky workarounds due to using a
;; setter for the `h/gateway-process-type' option.  The setter gets called
;; unexpectedly early in the compilation and/or load process, which causes
;; errors if the functions/methods and variables involved are not yet defined.
;; So we define the variable first, giving it a nil value, and define a default
;; for the running-p method (because the setter gets called before the option is
;; given its default value), and then the variable is redefined as an option and
;; given its default value.

(defvar h/gateway-process-type nil)

(cl-defmethod h//gateway-running-p ()
  "Return non-nil if the gateway process is running.")

(cl-defmethod h//gateway-running-p (&context (h/gateway-process-type (eql 'systemd)))
  "Return non-nil if the gateway process is running.
This does not mean that the gateway is responsive, only that the
process is running.  Used when HYPERDRIVE-GATEWAY-PROCESS-TYPE
is the symbol `systemd'."
  (zerop (call-process "systemctl" nil nil nil
                       "--user" "is-active" "hyper-gateway.service")))

(cl-defmethod h//gateway-running-p (&context (h/gateway-process-type (eql 'subprocess)))
  "Return non-nil if the gateway process is running.
This does not mean that the gateway is responsive, only that the
process is running.  Used when HYPERDRIVE-GATEWAY-PROCESS-TYPE
is the symbol `subprocess'."
  (process-live-p h/gateway-process))

(defcustom h/gateway-process-type nil
  "How to run the gateway process."
  ;; TODO: Can or should we use the :initialize function here?
  :set (lambda (option value)
         "Stop the gateway process before changing the type."
         (let ((value-changing-p (not (equal h/gateway-process-type value))))
           (unless value
             ;; Try to autodetect whether the gateway is already installed as a
             ;; systemd service.  (If systemd is not installed, it will default to
             ;; `subprocess'.)
             (setf value
                   (if (ignore-errors
                         (zerop (call-process "systemctl" nil nil nil
                                              "--user" "is-enabled" "hyper-gateway.service")))
                       'systemd
                     'subprocess)))
           (let ((runningp (h//gateway-running-p)))
             (when (and runningp value-changing-p)
               (h//gateway-stop))
             (set-default option value)
             (when (and runningp value-changing-p)
               (h//gateway-start)))))
  :type '(choice (const :tag "systemd service" systemd)
                 (const :tag "Emacs subprocess"
                        :description "When Emacs exits, the gateway will be terminated."
                        subprocess)
                 (const :tag "Autodetect" nil))
  :group 'hyperdrive)

(defcustom h/gateway-command "hyper-gateway --writable true --silent true run"
  ;; TODO: File Emacs bug report because the customization formatter handles the
  ;; "symbol `subprocess'" part differently than `describe-variable' does.
  "Command used to run the hyper-gateway.
Only used when `hyperdrive-gateway-process-type' is the symbol `subprocess'."
  :type 'string
  :group 'hyperdrive)

(cl-defmethod h//gateway-start (&context (h/gateway-process-type (eql 'systemd)))
  "Start the gateway as a systemd service.
Used when HYPERDRIVE-GATEWAY-PROCESS-TYPE is the symbol
`systemd'."
  (when (h//gateway-running-p)
    (user-error "Gateway already running"))
  (let ((buffer (get-buffer-create " *hyperdrive-start*")))
    (unwind-protect
        (unless (zerop (call-process "systemctl" nil (list buffer t) nil
                                     "--user" "start" "hyper-gateway.service"))
          (h/error "Unable to start hyper-gateway: %S"
                   (with-current-buffer buffer
                     (string-trim-right (buffer-string)))))
      (kill-buffer buffer))))

(cl-defmethod h//gateway-start (&context (h/gateway-process-type (eql 'subprocess)))
  "Start the gateway as an Emacs subprocess.
Used when HYPERDRIVE-GATEWAY-PROCESS-TYPE is the symbol
`subprocess'."
  (when (h//gateway-running-p)
    (user-error "Gateway already running"))
  (setf h/gateway-process
        (make-process :name "hyper-gateway"
                      :buffer " *hyperdrive-start*"
                      :command (split-string-and-unquote h/gateway-command)
                      :connection-type 'pipe))
  (sleep-for 0.5)
  (unless (process-live-p h/gateway-process)
    (if (h/status)
        (user-error "Gateway is already running outside of Emacs (see option `hyperdrive-gateway-process-type')")
      (pop-to-buffer " *hyperdrive-start*")
      (h/error "Gateway failed to start (see process buffer for errors)"))))

(cl-defmethod h//gateway-stop (&context (h/gateway-process-type (eql 'systemd)))
  "Stop the gateway service.
Used when HYPERDRIVE-GATEWAY-PROCESS-TYPE is the symbol
`systemd'."
  (unless (h//gateway-running-p)
    (user-error "Gateway not running"))
  (let ((buffer (get-buffer-create " *hyperdrive-stop*")))
    (unwind-protect
        (unless (zerop (call-process "systemctl" nil (list buffer t) nil
                                     "--user" "stop" "hyper-gateway.service"))
          (h/error "Unable to stop hyper-gateway: %S"
                   (with-current-buffer buffer
                     (string-trim-right (buffer-string)))))
      (cl-loop for i below 40
               do (sleep-for 0.1)
               while (h//gateway-running-p))
      (when (h//gateway-running-p)
        (h/error "Gateway still running"))
      (kill-buffer buffer))))

(cl-defmethod h//gateway-stop (&context (h/gateway-process-type (eql 'subprocess)))
  "Stop the gateway subprocess.
Used when HYPERDRIVE-GATEWAY-PROCESS-TYPE is the symbol
`subprocess'."
  (unless (h//gateway-running-p)
    (user-error "Gateway not running"))
  (interrupt-process h/gateway-process)
  (cl-loop for i below 40
           do (sleep-for 0.1)
           while (h//gateway-running-p))
  (when (h//gateway-running-p)
    (h/error "Gateway still running"))
  (kill-buffer (process-buffer h/gateway-process))
  (setf h/gateway-process nil))

;;;; Misc.

(defun h//get-buffer-create (entry)
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
  (let* ((buffer-name (h//format-entry
                       entry h/buffer-name-format))
         (buffer
          (or (when (eq 'any-version h/reuse-buffers)
                (cl-loop for buffer in (buffer-list)
                         when (h//buffer-visiting-entry-p buffer entry)
                         return buffer))
              (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      (rename-buffer buffer-name)
      ;; NOTE: We do not erase the buffer because, e.g. the directory
      ;; handler needs to record point before it erases the buffer.
      (if (h//entry-directory-p entry)
          (h/dir-mode)
        (when h/honor-auto-mode-alist
          ;; Inspired by https://emacs.stackexchange.com/a/2555/39549
          (let ((buffer-file-name (he/name entry)))
            (set-auto-mode))))
      (h/mode)
      (setq-local h/current-entry entry)
      (current-buffer))))

(defun h//buffer-visiting-entry-p (buffer entry)
  "Return non-nil when BUFFER is visiting ENTRY."
  (and (buffer-local-value 'h/current-entry buffer)
       (he/equal-p
        entry (buffer-local-value 'h/current-entry buffer))))

(defun h//buffer-for-entry (entry)
  "Return a predicate to match buffer against ENTRY."
  ;; TODO: This function is a workaround for bug#65797
  (lambda (buffer) (h//buffer-visiting-entry-p buffer entry)))

(defun h//format-entry (entry &optional format formats)
  "Return ENTRY formatted according to FORMAT.
FORMAT is a `format-spec' specifier string which maps to specifications
according to FORMATS, by default `hyperdrive-formats', which see."
  (pcase-let* (((cl-struct hyperdrive-entry hyperdrive name path version) entry)
               (formats (or formats h/formats)))
    (cl-labels ((fmt (format value)
                  (if value
                      (format (alist-get format formats) value)
                    "")))
      (propertize
       (format-spec (or format h/default-entry-format)
                    `((?n . ,(fmt 'name name))
                      (?p . ,(fmt 'path path))
                      (?v . ,(fmt 'version version))
                      (?H . ,(h//preferred-format hyperdrive nil formats))
                      (?D . ,(h//format hyperdrive "%D" formats))
                      (?k . ,(h//format hyperdrive "%k" formats))
                      (?K . ,(h//format hyperdrive "%K" formats))
                      (?N . ,(h//format hyperdrive "%N" formats))
                      (?P . ,(h//format hyperdrive "%P" formats))
                      (?S . ,(h//format hyperdrive "%S" formats))))
       'help-echo (he/url entry)))))

(defun h//entry-directory-p (entry)
  "Return non-nil if ENTRY is a directory."
  (string-suffix-p "/" (he/path entry)))

(defun h/message (message &rest args)
  "Call `message' with MESSAGE and ARGS, prefixing MESSAGE with \"Hyperdrive:\"."
  (apply #'message
         (concat "Hyperdrive: " (substitute-command-keys message)) args))

(defun h/user-error (format &rest args)
  "Call `user-error' with FORMAT and ARGS, prefixing FORMAT with \"Hyperdrive:\"."
  (apply #'user-error
         (concat "Hyperdrive: " (substitute-command-keys format)) args))

(defun h/insert-button (text &rest properties)
  "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
  ;; Inspired by package.el's `package-make-button'.
  (let ((button-text (if (display-graphic-p) text (concat "[" text "]")))
        (button-face (if (display-graphic-p) 'hyperdrive-button 'link)))
    (apply #'insert-text-button button-text 'face button-face 'follow-link t
           properties)))

(defun h/copy-tree (tree &optional vecp)
  "Copy TREE like `copy-tree', but with VECP, works for records too."
  ;; TODO: Now that the new copy-tree behavior has been merged into Emacs,
  ;; remove this function once compat.el supports the new behavior.
  (if (consp tree)
      (let (result)
	(while (consp tree)
	  (let ((newcar (car tree)))
	    (if (or (consp (car tree)) (and vecp (or (vectorp (car tree)) (recordp (car tree)))))
		(setq newcar (h/copy-tree (car tree) vecp)))
	    (push newcar result))
	  (setq tree (cdr tree)))
	(nconc (nreverse result)
               (if (and vecp (or (vectorp tree) (recordp tree))) (h/copy-tree tree vecp) tree)))
    (if (and vecp (or (vectorp tree) (recordp tree)))
	(let ((i (length (setq tree (copy-sequence tree)))))
	  (while (>= (setq i (1- i)) 0)
	    (aset tree i (h/copy-tree (aref tree i) vecp)))
	  tree)
      tree)))

(cl-defun h//format-path (path &key directoryp)
  "Return PATH with a leading slash if it lacks one.
When DIRECTORYP, also add a trailing slash to PATH if it lacks one.
When PATH is nil or blank, return \"/\"."
  (if (or (not path) (string-blank-p path))
      "/"
    (expand-file-name (if directoryp
                          (file-name-as-directory path)
                        path)
                      "/")))

;;;; Utilities

(defun h/time-greater-p (a b)
  "Return non-nil if time value A is greater than B."
  (not (or (time-less-p a b)
           (time-equal-p a b))))

(defun h//clean-buffer (&optional buffer)
  "Remove all local variables, overlays, and text properties in BUFFER.
When BUFFER is nil, act on current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (if (>= emacs-major-version 29)
        (with-suppressed-warnings ((callargs kill-all-local-variables))
          (kill-all-local-variables t))
      ;; NOTE: On Emacs <29, this function will not kill permanent-local
      ;; variables.  We're not sure if that will be a problem.
      (kill-all-local-variables))
    (let ((inhibit-read-only t))
      (delete-all-overlays)
      (set-text-properties (point-min) (point-max) nil))))

(defun he/equal-p (a b)
  "Return non-nil if hyperdrive entries A and B are equal.
Compares only public key, version, and path."
  (pcase-let (((cl-struct hyperdrive-entry (path a-path) (version a-version)
                          (hyperdrive (cl-struct hyperdrive (public-key a-key))))
               a)
              ((cl-struct hyperdrive-entry (path b-path) (version b-version)
                          (hyperdrive (cl-struct hyperdrive (public-key b-key))) )
               b))
    (and (eq a-version b-version)
         (equal a-path b-path)
         (equal a-key b-key))))

(defun h/equal-p (a b)
  "Return non-nil if hyperdrives A and B are equal.
Compares their public keys."
  (equal (h/public-key a) (h/public-key b)))

(defun he/hyperdrive-equal-p (a b)
  "Return non-nil if entries A and B have the same hyperdrive."
  (h/equal-p (he/hyperdrive a) (he/hyperdrive b)))

(defun h//ensure-dot-slash-prefix-path (path)
  "Return PATH, ensuring it begins with the correct prefix.
Unless PATH starts with \"/\" \"./\" or \"../\", add \"./\"."
  (if (string-match-p (rx bos (or "/" "./" "../")) path)
      path
    (concat "./" path)))

(provide 'hyperdrive-lib)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive-lib.el ends here
