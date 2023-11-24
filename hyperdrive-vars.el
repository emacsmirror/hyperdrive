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

(defcustom h/hyper-gateway-port 4973
  "Port on which to run the hyper-gateway server."
  :type 'natnum)

(defcustom h/honor-auto-mode-alist t
  "If non-nil, use file extension of hyperdrive file to set `major-mode'."
  :type 'boolean)

(defcustom h/persist-location nil
  ;; TODO: Consider using XDG locations for this, as well as storing
  ;; -hyperdrives separately from -version-ranges.  (Note that
  ;; xdg-state-home is only in Emacs 29+ and is not in compat.)
  "Location where `persist' will store data.

- `hyperdrive-hyperdrives'
- `hyperdrive-version-ranges'"
  :type '(choice (const :tag "Use default persist location" nil)
                 (file :tag "Custom location")))

(defcustom h/download-directory
  (expand-file-name
   (if (bound-and-true-p eww-download-directory)
       (if (stringp eww-download-directory)
           eww-download-directory
         (funcall eww-download-directory))
     "~/"))
  "Location where `hyperdrive-download-url' will download files.
Defaults to `eww-download-directory'."
  :type '(file :must-match t))

(defvar h/timestamp-width)
(defcustom h/timestamp-format "%x %X"
  "Format string used for timestamps.
Passed to `format-time-string', which see."
  :type 'string
  :set (lambda (option value)
         (set-default option value)
         (setf h/timestamp-width
               ;; FIXME: This value varies based on current
               ;;        time. (format-time-string "%-I") will
               ;;        be one or two characters long
               ;;        depending on the time of day
               (string-width (format-time-string value)))))

(defcustom h/directory-display-buffer-action
  '(display-buffer-same-window)
  "Display buffer action for hyperdrive directories.
Passed to `display-buffer', which see."
  ;; TODO: Perhaps use `display-buffer--action-custom-type'?
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "Pop up window" (display-buffer-pop-up-window))
                 (sexp :tag "Other")))

(defcustom h/directory-sort '(name . :ascending)
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

(defcustom h/history-display-buffer-action
  '(display-buffer-same-window)
  "Display buffer action for hyperdrive history buffers.
Passed to `display-buffer', which see."
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "Pop up window" (display-buffer-pop-up-window))
                 (sexp :tag "Other")))

(defcustom h/stream-player-command "mpv --force-window=immediate %s"
  "Command used to play streamable URLs externally.
In the command, \"%s\" is replaced with the URL (it should not be
quoted, because the arguments are passed directly rather than
through a shell)."
  :type '(choice (const :tag "MPV" "mpv --force-window=immediate %s")
                 (const :tag "VLC" "vlc %s")
                 (string :tag "Other command")))

(defcustom h/queue-limit 20
  "Default size of request queues."
  ;; TODO: Consider a separate option for metadata queue size (e.g. used in the dir handler).
  ;; TODO: Consider a separate option for upload queue size, etc.
  :type 'natnum)

(defcustom h/fill-version-ranges-limit 100
  "Default maximum number of requests when filling version history."
  :type 'natnum)

(defcustom h/render-html t
  "Render HTML hyperdrive files with EWW."
  :type 'boolean)

(defcustom h/reuse-buffers 'any-version
  "How to reuse buffers when showing entries.
When \\+`any-version', try to reuse an existing buffer showing the
same entry at any version.  When \\+`same-version', try to reuse
an existing buffer at the same version, or make a new buffer."
  :type '(choice (const :tag "Use an existing buffer at any version" any-version)
                 (const :tag "Use an existing buffer at the same version" same-version)))

;;;;;; Entry formatting

(defgroup hyperdrive-entry-format nil
  "Formatting of entries for buffer names, etc."
  :group 'hyperdrive)

(defcustom h/preferred-formats
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

(defcustom h/default-entry-format "[%H] %p%v"
  "Format string for displaying entries.
Specifiers:

%H  Preferred hyperdrive naming (see `hyperdrive-preferred-formats')

To configure the format of the following specifiers, see `hyperdrive-formats':

%n  Entry name
%p  Entry path
%v  Entry version
%S  Hyperdrive seed
%P  Hyperdrive petname
%N  Hyperdrive nickname
%K  Hyperdrive public key (full)
%k  Hyperdrive public key (short)
%D  Hyperdrive domains"
  :type 'string)

(defvar h/default-entry-format-without-version "[%H] %p"
  "Format string for displaying entries without displaying the version.
The format of the following specifiers can be configured using
`hyperdrive-formats', which see.")

(defcustom h/buffer-name-format "[%H] %n%v"
  "Format string for buffer names.
Specifiers are as in `hyperdrive-default-entry-format', which
see."
  :type 'string)

(defvar h/raw-formats '(;; Entry metadata
                        (name    . "%s")
                        (path    . "%s")
                        (version . "%s")
                        ;; Hyperdrive metadata
                        (petname    . "%s")
                        (nickname   . "%s")
                        (public-key . "%s")
                        (short-key  . "%s")
                        (seed       . "%s")
                        (domains    . "%s"))
  "Like `hyperdrive-formats', without any special formatting.")

(defcustom h/formats '(;; Entry metadata
                       (name       . "%s")
                       (version    . " (version:%s)")
                       (path       . "%s")
                       ;; Hyperdrive metadata
                       (petname    . "petname:%s")
                       (nickname   . "nickname:%s")
                       (public-key . "public-key:%s")
                       (short-key  . "public-key:%.8s…")
                       (seed       . "seed:%s")
                       (domains    . "domains:%s"))
  "Alist mapping hyperdrive and hyperdrive entry metadata item to format string.
Each metadata item may be one of:

- \\=`name' (Entry name)
- \\=`path' (Entry path)
- \\=`version' (Entry version)
- \\=`petname' (Hyperdrive petname)
- \\=`nickname' (Hyperdrive nickname)
- \\=`domains' (Hyperdrive domains)
- \\=`public-key' (Hyperdrive public key)
- \\=`short-key' (Hyperdrive short key)
- \\=`seed' (Hyperdrive seed)

In each corresponding format string, \"%s\" is replaced with the
value (and should only be present once in the string).  Used in
`hyperdrive-buffer-name-format', which see."
  :type '(list (cons :tag "Entry name" (const name)
                     (string :tag "Format string"))
               (cons :tag "Entry version" (const version)
                     (string :tag "Format string"))
               (cons :tag "Entry path" (const path)
                     (string :tag "Format string"))
               (cons :tag "Hyperdrive petname" (const petname)
                     (string :tag "Format string"))
               (cons :tag "Hyperdrive nickname" (const nickname)
                     (string :tag "Format string"))
               (cons :tag "Hyperdrive public key" (const public-key)
                     (string :tag "Format string"))
               (cons :tag "Hyperdrive short key" (const short-key)
                     (string :tag "Format string"))
               (cons :tag "Hyperdrive seed" (const seed)
                     (string :tag "Format string"))
               (cons :tag "Hyperdrive domains" (const domains)
                     (string :tag "Format string"))))

;;;;; Faces

(defgroup hyperdrive-faces nil
  "Faces shown in directory listings."
  :group 'hyperdrive)

(defface h/petname '((t :inherit font-lock-type-face))
  "Applied to hyperdrive petnames.")

(defface h/seed '((t :inherit font-lock-doc-face))
  "Applied to hyperdrive seeds.")

(defface h/domain '((t :inherit font-lock-keyword-face))
  "Applied to hyperdrive domains.")

(defface h/nickname '((t :inherit font-lock-warning-face))
  "Applied to hyperdrive nicknames.")

(defface h/public-key '((t :inherit font-lock-function-name-face))
  "Applied to hyperdrive public keys.")

(defface h/file-name '((t :inherit font-lock-keyword-face)) ; TODO theme
  "Applied to file names.")

(defface h/dimmed '((t :inherit shadow))
  "Applied to text in transient menus that should be dimmed.")

(defface h/header '((t (:inherit dired-header)))
  "Directory path.")

(defface h/column-header '((t (:inherit underline)))
  "Column header.")

(defface h/selected-column-header '((t ( :inherit underline
                                         :weight bold)))
  "Selected column header.")

(defface h/directory '((t (:inherit dired-directory)))
  "Subdirectories.")

(defface h/size '((t (:inherit font-lock-doc-face)))
  "Size of entries.")

(defface h/timestamp '((t (:inherit default)))
  "Entry timestamp.")

(defface h/header-arrow '((t (:inherit bold)))
  "Header arrows.")

(defface h/history-range '((t (:inherit font-lock-escape-face)))
  "Version range in `hyperdrive-history' buffers.")

(defface h/history-existent '((t :inherit success))
  "Marker for known existent entries in `hyperdrive-history'buffers.")

(defface h/history-nonexistent '((t :inherit error))
  "Marker for known nonexistent entries in `hyperdrive-history'buffers.")

(defface h/history-unknown '((t :inherit warning))
  "Marker for entries with unknown existence in `hyperdrive-history' buffers.")

;;;;; Regular expressions

(eval-and-compile
  (defconst h//hyper-prefix "hyper://"
    "Hyperdrive URL prefix."))

(defconst h//public-key-re
  (rx (eval h//hyper-prefix) (group (= 52 alphanumeric)))
  "Regexp to match \"hyper://\" + public key.

Capture group matches public key.")

(defconst h//version-re
  (rx (eval h//hyper-prefix)
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
(persist-defvar h/hyperdrives nil
                "List of known hyperdrives."
                h/persist-location)
(unless h/hyperdrives
  (setf h/hyperdrives (make-hash-table :test #'equal)))

(persist-defvar h/version-ranges nil
                "Hash table of hyperdrive version ranges.
Keys are generated by `hyperdrive--entry-version-range-key', and
values are alists mapping version range starts to plists with
`:existsp' and `:range-end' keys."
                h/persist-location)
(unless h/version-ranges
  (setf h/version-ranges (make-hash-table :test #'equal)))

;; TODO: Flesh out the persist hook.
;; (defvar hyperdrive-persist-hook nil
;;   :type 'hook)

;;;;; Internals

(defvar-local h/current-entry nil
  "Entry for current buffer.")
(put 'h/current-entry 'permanent-local t)

(defvar h/type-handlers
  `(
    ;; Directories are sent from the gateway as JSON arrays
    ("application/json" . h/handler-json)
    (,(rx bos "audio/") . h/handler-streamable)
    (,(rx bos "video/") . h/handler-streamable)
    (,(rx bos "image/") . h/handler-image)
    (,(rx (or "text/html" "application/xhtml+xml")) . h/handler-html))
  "Alist mapping MIME types to handler functions.
Keys are regexps matched against MIME types.")

(defvar h/dir-sort-fields
  '((size  :accessor he/size
           :ascending <
           :descending >
           :desc "Size")
    (mtime :accessor he/mtime
           :ascending time-less-p
           :descending h/time-greater-p
           :desc "Last Modified")
    (name  :accessor he/name
           :ascending string<
           :descending string>
           :desc "Name"))
  "Fields for sorting hyperdrive directory buffer columns.")

;;;; Footer

(provide 'hyperdrive-vars)

;;;###autoload(register-definition-prefixes "hyperdrive-vars" '("hyperdrive-"))
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:
;;; hyperdrive-vars.el ends here
