;;; hyperdrive.el --- Emacs gateway to the Hypercore network  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joseph Turner <joseph@ushin.org>

;; Author: Joseph Turner
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <joseph@ushin.org>
;; Created: 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3") (map "3.0") (compat "29.1.3.2") (plz "0.3") (mpv "0.2.0") (persist "0.5"))
;; Homepage: https://git.sr.ht/~ushin/hyperdrive.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; hyperdrive.el integrates with `hyper-gateway' for sharing files on the
;; https://hypercore-protocol.org network.

;;;; Installation:

;;;;; Dependencies

;;;;;; hyper-gateway

;; hyperdrive.el relies on
;; [hyper-gateway](https://github.com/RangerMauve/hyper-gateway/) for
;; talking to the hypercore network.

;; Download or compile the hyper-gateway
;; (https://github.com/RangerMauve/hyper-gateway/releases) binary and
;; ensure that it is executable and in your $PATH.

;; Ensure that `hyperdrive-hyper-gateway-command' is set to the name
;; you gave to the `hyper-gateway` binary. One way to do this is by
;; renaming the binary to `hyper-gateway`, the default value for
;; `hyperdrive-hyper-gateway-command'.

;;;;;; plz.el

;; hyperdrive.el uses [plz.el](https://github.com/alphapapa/plz.el) for sending HTTP requests to hyper-gateway.

;;;;;; mpv.el

;; hyperdrive.el uses [mpv.el](https://github.com/kljohann/mpv.el) for streaming audio and video.

;;;;;; compat.el

;; hyperdrive.el relies on [compat.el](https://github.com/emacs-compat/compat) to support Emacs versions prior to Emacs 29.

;;;;; Manual

;; Clone this repository:

;; git clone https://git.sr.ht/~ushin/hyperdrive.el/ ~/.local/src/hyperdrive.el/

;; Add the following lines to your init.el file:

;; (add-to-list 'load-path "~/.local/src/hyperdrive.el")
;; (require 'hyperdrive)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'rx)

;; NOTE: On older versions of Emacs with older built-in versions of
;; `map', it's not possible to force Emacs to load the newer version
;; of the library when compiling the package; we can only
;; "Package-Requires" it to cause it to be installed.  So in the case
;; that a user encounters an error caused by macro-expanding an older
;; version of the library, the user would need to delete the
;; hyperdrive package, restart Emacs, and reinstall this package to
;; fix it.
(require 'map)

(require 'compat)
(require 'plz)
(require 'mpv)
(require 'persist)

(require 'hyperdrive-lib)
(require 'hyperdrive-handlers)
(require 'hyperdrive-org)

(defgroup hyperdrive nil
  "Emacs gateway to the Hypercore network."
  :group 'communication
  :group 'external
  :prefix "hyperdrive-")

;;;; Configuration

(defcustom hyperdrive-storage-location
  (expand-file-name "~/.local/share/hyper-gateway-nodejs/")
  "Location to store Hypercore data."
  :type '(file :must-match t))

(defcustom hyperdrive-hyper-gateway-command "hyper-gateway"
  "Name of `hyper-gateway' binary on your machine."
  :type 'string)

(defcustom hyperdrive-hyper-gateway-port 4973
  "Port on which to run the hyper-gateway server."
  :type 'natnum)

(defcustom hyperdrive-hyper-gateway-p2p-port 4977
  "Port on which to run the p2p network."
  :type 'natnum)

(defcustom hyperdrive-honor-auto-mode-alist t
  "If non-nil, use file extension of hyperdrive file to set `major-mode'."
  :type 'boolean)

(defcustom hyperdrive-persist-location nil
  "Location where `persist' will store data."
  :type '(choice (const :tag "Use default persist location" nil)
                 (file :tag "Custom location")))

(defcustom hyperdrive-download-directory "~/"
  "Location where `hyperdrive-download-url-as-file' will download files."
  :type '(file :must-match t))

(defcustom hyperdrive-timestamp-format "%x %X"
  "Format string used for timestamps.
Passed to `format-time-string', which see."
  :type 'string
  :set (lambda (option value)
         (set option value)
         (setf hyperdrive-timestamp-format-string
               (format "%%%ss"
                       (string-width (format-time-string value))))))

(defcustom hyperdrive-directory-display-buffer-action
  '(display-buffer-same-window)
  "Display buffer action for hyperdrive directories.
Passed to `display-buffer', which see."
  :type '(choice (const :tag "Same window" (display-buffer-same-window))
                 (const :tag "Pop up window" (display-buffer-pop-up-window))
                 (sexp :tag "Other")))

;;;; Internal variables

(persist-defvar hyperdrive--namespaces nil
                ;; TODO: Refactor this into hyperdrive-hyperdrives.
                "List of cons pairs mapping an alias to a public key."
                hyperdrive-persist-location)

(persist-defvar hyperdrive-hyperdrives nil
                "List of known hyperdrives."
                hyperdrive-persist-location)

(defconst hyperdrive-org-link-type "hyper" "Org mode link type.")

(defvar-local hyperdrive-current-entry nil
  "Entry for current buffer.")
(put 'hyperdrive-current-entry 'permanent-local t)

;;;; User interaction helper functions

;;;; Commands

;; (defun hyperdrive-public-key (alias)
;;   "Copy the formatted public key corresponding to ALIAS to kill-ring."
;;   (interactive (list (hyperdrive--completing-read-alias)))
;;   (let* ((public-key (hyperdrive--public-key-by-alias alias))
;;          (formatted-key (hyperdrive--make-hyperdrive-url public-key "")))
;;     (message "%s" formatted-key)
;;     (kill-new formatted-key)))

(defun hyperdrive--gateway-pid ()
  "Return `hyper-gateway' process id if it's running. Otherwise, return nil."
  (let ((output
         (shell-command-to-string (concat "pgrep " hyperdrive-hyper-gateway-command))))
    (when (> (length output) 0)
      (string-to-number (string-trim output)))))

(defun hyperdrive--gateway-ready-p ()
  "Return non-nil if hyper-gateway is ready."
  (let (readyp)
    (hyperdrive-api 'get "hyper://localhost"
      ;; FIXME: Don't use else handler, since plz should not call it after a synchronous request
      :else (lambda (err)
              (unless (and (plz-error-curl-error err)
                           ;; "Failed to connect to host."
                           (= 7 (car (plz-error-curl-error err))))
                ;; Status code 400 is expected when hyper-gateway is running
                ;; See https://github.com/RangerMauve/hyper-gateway/issues/3
                (when (= 400 (plz-response-status (plz-error-response err)))
                  (setq readyp t)))))
    readyp))

(defun hyperdrive--alias-url (alias)
  "Return URL to hyperdrive known as ALIAS, or nil if it doesn't exist.
That is, if the ALIAS has been used to create a local
hyperdrive."
  ;; TODO: Should this function go inside hyperdrive-lib.el?
  (condition-case err
      (pcase (hyperdrive-api 'get (concat "hyper://localhost/?key=" (url-hexify-string alias))
               :as 'response)
        ((and (pred plz-response-p)
              response
              (guard (= 200 (plz-response-status response))))
         (plz-response-body response)))
    (plz-http-error (if (= 400 (plz-response-status (plz-error-response (cdr err))))
                        nil
                      (signal 'plz-http-error err)))))

;;;###autoload
(defun hyperdrive-start ()
  "Start `hyper-gateway' if not already running."
  (interactive)
  (unless (hyperdrive--gateway-ready-p)
    (let ((buf (get-buffer-create " *hyper-gateway*")))
      (with-current-buffer buf (erase-buffer))
      (make-process
       :name "hyper-gateway"
       :buffer buf
       :command (list hyperdrive-hyper-gateway-command "--writable" "true" "run")))))

(defun hyperdrive-stop ()
  "Stop the `hyper-gateway' process."
  (interactive)
  (let ((proc (hyperdrive--gateway-pid)))
    (if proc
        (signal-process proc 'sigint)
      (message "Already not running hyper-gateway."))))

;; (defun hyperdrive-upload-files (alias relative-dir files)
;; TODO: Update this.
;;   "Upload files from the local filesystem to the hyperdrive for ALIAS.

;; RELATIVE-DIR is the directory relative to which files in the
;; hyperdrive will be inserted.

;; FILES can be either be a list of filepaths (strings) or a
;; function which returns a list of filepaths."
;;   (let ((file-list
;;          (if (functionp files)
;;              (funcall files)
;;            files)))
;;     (dolist (path file-list)
;;       (with-temp-buffer
;;         (insert-file-contents path)
;;         (hyperdrive-save-buffer-by-alias
;;          alias (concat "/" (file-relative-name path relative-dir)))))))

(defun hyperdrive-create-namespace (alias)
  "Create a hyperdrive namespace from an alphanumeric ALIAS.

This adds a new cons pair to `hyperdrive--namespaces'.

This function is idempotent; running it multiple times with the
same ALIAS does not create a new namespace."
  (interactive "sNamespace alias: ")
  (let ((public-key (hyperdrive--extract-public-key
                     (plz-response-body
                      (hyperdrive-api 'post (concat hyperdrive--hyper-prefix "localhost/?key=" alias)
                        :as 'response)))))
    (cl-pushnew (cons alias public-key) hyperdrive--namespaces :test #'equal)
    (message "%s: %s" alias (hyperdrive--make-hyperdrive-url public-key ""))))

;; (defun hyperdrive-load-alias (alias)
;;   ;; TODO: Update this.
;;   "Load hyperdrive corresponding to ALIAS."
;;   (interactive (list (hyperdrive--completing-read-alias)))
;;   (hyperdrive--load-url-directory
;;    (hyperdrive--make-hyperdrive-url (hyperdrive--public-key-by-alias alias) "")))

;; (defun hyperdrive-download-url-as-file (url filename)
;;   "Load contents at URL as a file to store at FILENAME.

;; URL should begin with `hyperdrive--hyper-prefix'."
;;   (interactive
;;    (let* ((read-url (read-string "URL: "))
;;           (read-filename (read-string "Filename: "
;;                                       (expand-file-name
;;                                        (file-name-nondirectory
;;                                         (hyperdrive--extract-path read-url))
;;                                        hyperdrive-download-directory))))
;;      (list read-url read-filename)))
;;   (hyperdrive-api 'get url :as `(file ,filename)))

(defun hyperdrive-revert-buffer (&optional _arg _noconfirm)
  "Revert `hyperdrive-mode' buffer by reloading hyperdrive contents."
  ;; TODO: Override buffer-modified check when buffer is erased.
  (hyperdrive-open (hyperdrive-entry-url hyperdrive-current-entry)))

;;;; hyperdrive-mode

(defun hyperdrive-mode-on ()
  "Activate `hyperdrive-mode'."
  (setq-local revert-buffer-function #'hyperdrive-revert-buffer)
  (cl-pushnew #'hyperdrive--write-contents write-contents-functions))

(defun hyperdrive-mode-off ()
  "Deactivate `hyperdrive-mode'."
  (setq-local revert-buffer-function #'revert-buffer--default
              write-contents-functions (remove #'hyperdrive--write-contents write-contents-functions)))

(define-minor-mode hyperdrive-mode
  "Minor mode for buffers opened from hyperdrives."
  :global nil
  :interactive nil
  :group 'hyperdrive
  :lighter "hyperdrive"
  ;; :keymap (let ((map (make-sparse-keymap)))
  ;;           ;; TODO: Redo this command.
  ;;           (define-key map [remap dired-jump]  #'hyperdrive-up-directory)
  ;;           map)
  (if hyperdrive-mode
      (hyperdrive-mode-on)
    (hyperdrive-mode-off)))

;;;###autoload
(cl-defun hyperdrive-open (url &key then)
  "Open hyperdrive URL.
THEN may be a function to pass to the handler to call in the
buffer opened by the handler."
  (declare (indent defun))
  (interactive
   (list (hyperdrive-complete-url)))
  ;; TODO: Ensure gateway is running.
  (let ((entry (hyperdrive-url-entry url)))
    (hyperdrive-fill entry
      :then (lambda (entry)
              (pcase-let* (((cl-struct hyperdrive-entry type) entry)
                           ;; MAYBE: Use alist-get instead of cl-find-if.
                           (handler (or (cdr (cl-find-if (lambda (regexp)
                                                           (string-match-p regexp type))
                                                         hyperdrive-type-handlers :key #'car))
                                        #'hyperdrive-handler-default)))
                (funcall handler entry :then then)
                (hyperdrive-persist (hyperdrive-entry-hyperdrive entry))))
      :else (lambda (plz-error)
              (pcase-let* (((cl-struct plz-error response) plz-error)
                           ((cl-struct plz-response status) response))
                (pcase status
                  (404 (when (yes-or-no-p (format "URL not found: %S.  Try to load parent directory? " url))
                         (hyperdrive-open (hyperdrive--parent url))) )
                  (_ (hyperdrive-message "Unable to load URL %S: %S" url plz-error))))))))

(defun hyperdrive-save-buffer (entry)
  "Save ENTRY to hyperdrive (interactively, the current buffer).
If buffer was not hyperdrive-backed, it becomes so."
  (interactive
   (list (if hyperdrive-mode
             hyperdrive-current-entry
           (hyperdrive--read-new-entry))))
  (hyperdrive-write-buffer entry 'overwrite))

(defun hyperdrive-write-buffer (entry &optional overwritep)
  "Write current buffer to new hyperdrive ENTRY.
If file already exists and OVERWRITEP is nil, prompt the user to
overwrite."
  (interactive (list (hyperdrive--read-new-entry)))
  ;; FIXME: Overwrites without prompting if file exists.  Make new
  ;; --writable-p based on
  ;; <https://github.com/RangerMauve/hypercore-fetch/issues/60>.
  (ignore overwritep)
  (unless hyperdrive-mode
    (hyperdrive-mode)
    (setq-local hyperdrive-current-entry entry))
  (pcase-let (((cl-struct hyperdrive-entry name) entry)
              (url (hyperdrive-entry-url entry))
              (buffer (current-buffer)))
    (hyperdrive-write entry
      :body (save-restriction
              (widen)
              (buffer-substring-no-properties (point-min) (point-max)))
      :then (lambda (_response)
              ;; TODO: Fill entry after writing it (and e.g. display
              ;; new etag in mode line).
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (rename-buffer (hyperdrive--format-url url) 'unique)
                  (set-buffer-modified-p nil)))
              (hyperdrive-message "Wrote: %S to %S" name url))
      :else (lambda (plz-error)
              (pcase-let* (((cl-struct plz-error response) plz-error)
                           ((cl-struct plz-response status body) response)
                           ;; TODO: hyper-gateway should return 403
                           ;; when not writable.  See:
                           ;; <https://todo.sr.ht/~ushin/ushin/25>.
                           (message (if (and (eq 500 status)
                                             (string-match-p "SESSION_NOT_WRITABLE" body))
                                        "Hyperdrive not writable"
                                      plz-error)))
                (hyperdrive-message "Unable to write: %S: %S" name message))))
    (hyperdrive-message "Saving to %S..." url)))

(defun hyperdrive--write-contents ()
  "Call `hyperdrive-save-buffer' for the current buffer.
To be used in `write-contents-functions'."
  (cl-assert hyperdrive-mode)
  (hyperdrive-save-buffer hyperdrive-current-entry))

(defun hyperdrive-copy-url (hyperdrive)
  "Save HYPERDRIVE's URL to the kill ring.
Interactively, prompts for hyperdrive from
`hyperdrive-hyperdrives'."
  (interactive (list (hyperdrive-complete-hyperdrive)))
  (kill-new (hyperdrive-url hyperdrive))
  (message "%s" (hyperdrive-url hyperdrive)))

;;;; Footer

(provide 'hyperdrive)
;;; hyperdrive.el ends here
