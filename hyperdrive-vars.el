;;; hyperdrive-vars.el --- Shared (persist-)defvars, deffaces, defcustoms  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>

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

(require 'persist)
(require 'dired)     ; For faces.
(require 'cus-edit)  ; For `custom-button' face.

;;;; Configuration:

(defgroup hyperdrive nil
  "P2P filesystem in Emacs."
  :group 'communication
  :group 'external
  :prefix "hyperdrive-")

(defcustom hyperdrive-storage-location
  (expand-file-name "~/.local/share/hyper-gateway-nodejs/")
  "Location to store Hypercore data."
  :type '(file :must-match t))

(defcustom hyperdrive-hyper-gateway-port 4973
  "Port on which to run the hyper-gateway server."
  :type 'natnum)

(defcustom hyperdrive-honor-auto-mode-alist t
  "If non-nil, use file extension of hyperdrive file to set `major-mode'."
  :type 'boolean)

(defcustom hyperdrive-persist-location nil
  "Location where `persist' will store data.

- `hyperdrive-hyperdrives'
- `hyperdrive-version-ranges'"
  :type '(choice (const :tag "Use default persist location" nil)
                 (file :tag "Custom location")))

(defcustom hyperdrive-download-directory
  (expand-file-name
   (if (bound-and-true-p eww-download-directory)
       (if (stringp eww-download-directory)
           eww-download-directory
         (funcall eww-download-directory))
     "~/"))
  "Location where `hyperdrive-download-url' will download files.
Defaults to `eww-download-directory'."
  :type '(file :must-match t))

(defvar hyperdrive-timestamp-width)
(defcustom hyperdrive-timestamp-format "%x %X"
  "Format string used for timestamps.
Passed to `format-time-string', which see."
  :type 'string
  :set (lambda (option value)
         (set-default option value)
         (setf hyperdrive-timestamp-width
               ;; FIXME: This value varies based on current
               ;;        time. (format-time-string "%-I") will
               ;;        be one or two characters long
               ;;        depending on the time of day
               (string-width (format-time-string value)))))

(defcustom hyperdrive-directory-display-buffer-action
  '(display-buffer-same-window)
  "Display buffer action for hyperdrive directories.
Passed to `display-buffer', which see."
  ;; TODO: Perhaps use `display-buffer--action-custom-type'?
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "Pop up window" (display-buffer-pop-up-window))
                 (sexp :tag "Other")))

(defcustom hyperdrive-directory-sort '(name . :ascending)
  "Column by which directory entries are sorted.
Internally, a cons cell of (COLUMN . DIRECTION), the COLUMN being
one of the directory listing columns (\\+`name', \\+`size', or
\\+`mtime') and DIRECTION being one of \\+`:ascending' or
\\+`:descending'."
  :type '(radio (cons :tag "By name" (const :format "" name)
                      (choice :tag "Direction" :value :ascending
                              (const :tag "Ascending" :ascending)
                              (const :tag "Descending" :descending)))
                (cons :tag "By size" (const :format "" size)
                      (choice :tag "Direction" :value :ascending
                              (const :tag "Ascending" :ascending)
                              (const :tag "Descending" :descending)))
                (cons :tag "By date" (const :format "" mtime)
                      (choice :tag "Direction" :value :ascending
                              (const :tag "Ascending" :ascending)
                              (const :tag "Descending" :descending)))))

(defcustom hyperdrive-history-display-buffer-action
  '(display-buffer-same-window)
  "Display buffer action for hyperdrive history buffers.
Passed to `display-buffer', which see."
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "Pop up window" (display-buffer-pop-up-window))
                 (sexp :tag "Other")))

(defcustom hyperdrive-default-host-format
  '(petname nickname domain seed short-key public-key)
  "Default format for displaying hyperdrive hostnames.
Each option is checked in order, and the first available type is
used."
  :type '(repeat
          (choice (const :tag "Petname" petname)
                  (const :tag "Nickname"
                         :doc "(Nickname specified by hyperdrive author)"
                         :format "%t %h"
                         nickname)
                  (const :tag "DNSLink domain" domain)
                  (const :tag "Seed" seed)
                  (const :tag "Shortened public key" short-key)
                  (const :tag "Full public key" public-key))))

(defcustom hyperdrive-stream-player-command "mpv --force-window=immediate %s"
  "Command used to play streamable URLs externally.
In the command, \"%s\" is replaced with the URL (it should not be
quoted, because the arguments are passed directly rather than
through a shell)."
  :type '(choice (const :tag "MPV" "mpv --force-window=immediate %s")
                 (const :tag "VLC" "vlc %s")
                 (string :tag "Other command")))

(defcustom hyperdrive-queue-limit 20
  "Default size of request queues."
  ;; TODO: Consider a separate option for metadata queue size (e.g. used in the dir handler).
  ;; TODO: Consider a separate option for upload queue size, etc.
  :type 'natnum)

(defcustom hyperdrive-fill-version-ranges-limit 100
  "Default maximum number of requests when filling version history."
  :type 'natnum)

(defcustom hyperdrive-render-html t
  "Render HTML hyperdrive files with EWW."
  :type 'boolean)

(defcustom hyperdrive-reuse-buffers 'any-version
  "How to reuse buffers when showing entries.
When \\+`any-version', try to reuse an existing buffer showing the
same entry at any version.  When \\+`same-version', try to reuse
an existing buffer at the same version, or make a new buffer."
  :type '(choice (const :tag "Use an existing buffer at any version" any-version)
                 (const :tag "Use an existing buffer at the same version" same-version)))

;;;;; Faces

(defgroup hyperdrive-faces nil
  "Faces shown in directory listings."
  :group 'hyperdrive)

(defface hyperdrive-petname '((t :inherit font-lock-type-face))
  "Applied to hyperdrive petnames.")

(defface hyperdrive-seed '((t :inherit font-lock-doc-face))
  "Applied to hyperdrive seeds.")

(defface hyperdrive-domain '((t :inherit font-lock-keyword-face))
  "Applied to hyperdrive domains.")

(defface hyperdrive-nickname '((t :inherit font-lock-warning-face))
  "Applied to hyperdrive nicknames.")

(defface hyperdrive-public-key '((t :inherit font-lock-function-name-face))
  "Applied to hyperdrive public keys.")

(defface hyperdrive-header '((t (:inherit dired-header)))
  "Directory path.")

(defface hyperdrive-column-header '((t (:inherit underline)))
  "Column header.")

(defface hyperdrive-selected-column-header '((t ( :inherit underline
                                                  :weight bold)))
  "Selected column header.")

(defface hyperdrive-directory '((t (:inherit dired-directory)))
  "Subdirectories.")

(defface hyperdrive-size '((t (:inherit font-lock-doc-face)))
  "Size of entries.")

(defface hyperdrive-timestamp '((t (:inherit default)))
  "Entry timestamp.")

(defface hyperdrive-header-arrow '((t (:inherit bold)))
  "Header arrows.")

(defface hyperdrive-history-range '((t (:inherit font-lock-escape-face)))
  "Version range in `hyperdrive-history' buffers.")

(defface hyperdrive-history-existent '((t :inherit success))
  "Marker for known existent entries in `hyperdrive-history'buffers.")

(defface hyperdrive-history-nonexistent '((t :inherit error))
  "Marker for known nonexistent entries in `hyperdrive-history'buffers.")

(defface hyperdrive-history-unknown '((t :inherit warning))
  "Marker for entries with unknown existence in `hyperdrive-history' buffers.")

(defface hyperdrive-mirror-new '((t (:foreground "black" :background "green")))
  "Face for files with \"new\" status in `hyperdrive-mirror' buffers.")

(defface hyperdrive-mirror-same '((t (:foreground "black" :background "red")))
  "Face for files with \"same\" status in `hyperdrive-mirror' buffers.")

(defface hyperdrive-mirror-newer '((t (:foreground "black" :background "yellow")))
  "Face for files with \"newer\" status in `hyperdrive-mirror' buffers.")

(defface hyperdrive-mirror-older '((t (:foreground "black" :background "purple")))
  "Face for files with \"older\" status in `hyperdrive-mirror' buffers.")

;;;;; Regular expressions

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

;;;;; Persisted variables

;; NOTE: `persist' currently does not work correctly with hash tables
;; if the default value of a persisted variable is one; it considers
;; them equal at save time and so deletes the persisted variable file.
;; To work around this, we set the default value to nil and initialize
;; it to a hash table "manually".
;; TODO: See persist.el patch: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=63513>
(persist-defvar hyperdrive-hyperdrives nil
                "List of known hyperdrives."
                hyperdrive-persist-location)
(unless hyperdrive-hyperdrives
  (setf hyperdrive-hyperdrives (make-hash-table :test #'equal)))

(persist-defvar hyperdrive-version-ranges nil
                "Hash table of hyperdrive version ranges.
Keys are generated by `hyperdrive--entry-version-range-key', and
values are alists mapping version range starts to plists with
`:existsp' and `:range-end' keys."
                hyperdrive-persist-location)
(unless hyperdrive-version-ranges
  (setf hyperdrive-version-ranges (make-hash-table :test #'equal)))

;; TODO: Flesh out the persist hook.
;; (defvar hyperdrive-persist-hook nil
;;   :type 'hook)

;;;;; Internals

(defvar-local hyperdrive-current-entry nil
  "Entry for current buffer.")
(put 'hyperdrive-current-entry 'permanent-local t)

(defvar hyperdrive-type-handlers
  `(
    ;; Directories are sent from the gateway as JSON arrays
    ("application/json" . hyperdrive-handler-json)
    (,(rx bos "audio/") . hyperdrive-handler-streamable)
    (,(rx bos "video/") . hyperdrive-handler-streamable)
    (,(rx bos "image/") . hyperdrive-handler-image)
    (,(rx (or "text/html" "application/xhtml+xml")) . hyperdrive-handler-html))
  "Alist mapping MIME types to handler functions.
Keys are regexps matched against MIME types.")

(defvar hyperdrive-dir-sort-fields
  '((size  :accessor hyperdrive-entry-size
           :ascending <
           :descending >
           :desc "Size")
    (mtime :accessor hyperdrive-entry-mtime
           :ascending time-less-p
           :descending hyperdrive-time-greater-p
           :desc "Last Modified")
    (name  :accessor hyperdrive-entry-name
           :ascending string<
           :descending string>
           :desc "Name"))
  "Fields for sorting hyperdrive directory buffer columns.")

;;;; Footer

(provide 'hyperdrive-vars)
;;; hyperdrive-vars.el ends here
