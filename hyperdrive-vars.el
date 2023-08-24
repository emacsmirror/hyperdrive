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
  :type '(file :must-match t)
  :group 'hyperdrive)

(defcustom hyperdrive-hyper-gateway-port 4973
  "Port on which to run the hyper-gateway server."
  :type 'natnum
  :group 'hyperdrive)

(defcustom hyperdrive-honor-auto-mode-alist t
  "If non-nil, use file extension of hyperdrive file to set `major-mode'."
  :type 'boolean
  :group 'hyperdrive)

(defcustom hyperdrive-persist-location nil
  "Location where `persist' will store data."
  :type '(choice (const :tag "Use default persist location" nil)
                 (file :tag "Custom location"))
  :group 'hyperdrive)

(defcustom hyperdrive-download-directory (expand-file-name
                                          (if (bound-and-true-p eww-download-directory)
                                              (if (stringp eww-download-directory)
                                                  eww-download-directory
                                                (funcall eww-download-directory))
                                            "~/"))
  "Location where `hyperdrive-download-url' will download files.
Defaults to `eww-download-directory'."
  :type '(file :must-match t)
  :group 'hyperdrive)

(defvar hyperdrive-timestamp-format-string)
(defcustom hyperdrive-timestamp-format "%x %X"
  "Format string used for timestamps.
Passed to `format-time-string', which see."
  :type 'string
  :set (lambda (option value)
         (set option value)
         (setf hyperdrive-timestamp-format-string
               (format "%%%ds"
                       ;; FIXME: This value varies based on current
                       ;;        time. (format-time-string "%-I") will
                       ;;        be one or two characters long
                       ;;        depending on the time of day
                       (string-width (format-time-string value)))))
  :group 'hyperdrive)

(defcustom hyperdrive-directory-display-buffer-action
  '(display-buffer-same-window)
  "Display buffer action for hyperdrive directories.
Passed to `display-buffer', which see."
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "Pop up window" (display-buffer-pop-up-window))
                 (sexp :tag "Other"))
  :group 'hyperdrive)

(defcustom hyperdrive-history-display-buffer-action
  '(display-buffer-same-window)
  "Display buffer action for hyperdrive history buffers.
Passed to `display-buffer', which see."
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "Pop up window" (display-buffer-pop-up-window))
                 (sexp :tag "Other"))
  :group 'hyperdrive)

(defcustom hyperdrive-column-headers 't
  "Display column headers in `hyperdrive-dir' and `hyperdrive-history' buffers."
  :type 'boolean
  :group 'hyperdrive)

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
                  (const :tag "Full public key" public-key)))
  :group 'hyperdrive)

(defcustom hyperdrive-stream-player-command "mpv --force-window=immediate %s"
  "Command used to play streamable URLs externally.
In the command, \"%s\" is replaced with the URL (it should not be
quoted, because the arguments are passed directly rather than
through a shell)."
  :type '(choice (const :tag "MPV" "mpv --force-window=immediate %s")
                 (const :tag "VLC" "vlc %s")
                 (string :tag "Other command"))
  :group 'hyperdrive)

(defcustom hyperdrive-queue-size 2
  "Default size of request queues."
  ;; TODO: Use this elsewhere also.
  :type 'integer
  :group 'hyperdrive)

(defcustom hyperdrive-render-html t
  "Render HTML hyperdrive files with EWW."
  :type 'boolean
  :group 'hyperdrive)

;;;;; Faces

(defgroup hyperdrive-faces nil
  "Faces shown in directory listings."
  :group 'hyperdrive)

(defface hyperdrive-petname '((t :inherit font-lock-type-face))
  "Applied to hyperdrive petnames."
  :group 'hyperdrive-faces)

(defface hyperdrive-seed '((t :inherit font-lock-doc-face))
  "Applied to hyperdrive seeds."
  :group 'hyperdrive-faces)

(defface hyperdrive-domain '((t :inherit font-lock-keyword-face))
  "Applied to hyperdrive domains."
  :group 'hyperdrive-faces)

(defface hyperdrive-nickname '((t :inherit font-lock-warning-face))
  "Applied to hyperdrive nicknames."
  :group 'hyperdrive-faces)

(defface hyperdrive-public-key '((t :inherit font-lock-function-name-face))
  "Applied to hyperdrive public keys."
  :group 'hyperdrive-faces)

(defface hyperdrive-header
  '((t (:inherit dired-header)))
  "Directory path."
  :group 'hyperdrive-faces)

(defface hyperdrive-column-header
  '((t (:inherit underline)))
  "Directory path."
  :group 'hyperdrive-faces)

(defface hyperdrive-directory
  '((t (:inherit dired-directory)))
  "Subdirectories."
  :group 'hyperdrive-faces)

(defface hyperdrive-size
  '((t (:inherit font-lock-doc-face)))
  "Size of entries."
  :group 'hyperdrive-faces)

(defface hyperdrive-timestamp
  '((t (:inherit default)))
  "Entry timestamp."
  :group 'hyperdrive-faces)

(defface hyperdrive-history-range
  '((t (:inherit font-lock-escape-face)))
  "Version range in `hyperdrive-history' buffers."
  :group 'hyperdrive-faces)

(defface hyperdrive-history-existent '((t (:foreground "black" :background "green")))
  "Marker for known existent entries in `hyperdrive-history'buffers."
  :group 'hyperdrive-faces)

(defface hyperdrive-history-nonexistent '((t (:foreground "black" :background "red")))
  "Marker for known nonexistent entries in `hyperdrive-history'buffers."
  :group 'hyperdrive-faces)

(defface hyperdrive-history-unknown '((t (:foreground "black" :background "yellow")))
  "Marker for entries with unknown existence in `hyperdrive-history' buffers."
  :group 'hyperdrive-faces)

(defface hyperdrive-button
  ;; Inspired by cus-edit.el's `custom-button' face.
  ;; NOTE: This face is not currently used, but
  ;; `hyperdrive-button-dangerous' inherits from it.
  '((t (:inherit custom-button)))
  "Face for hyperdrive buttons."
  :group 'hyperdrive-faces)

(defface hyperdrive-button-dangerous
  '((t (:inherit hyperdrive-button
                 :box (:line-width 3)
                 :background "red" :foreground "yellow")))
  "Face for dangerous hyperdrive buttons."
  :group 'hyperdrive-faces)

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

;;;; Footer

(provide 'hyperdrive-vars)
;;; hyperdrive-vars.el ends here
