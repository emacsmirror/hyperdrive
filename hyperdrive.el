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

;;;;; Faces

(defface hyperdrive-alias '((t :inherit font-lock-doc-face))
  "Applied to hyperdrive aliases.")

(defface hyperdrive-public-key '((t :inherit font-lock-constant-face))
  "Applied to hyperdrive public keys.")

;;;; Internal variables

;; NOTE: `persist' currently does not work correctly with hash tables
;; if the default value of a persisted variable is one; it considers
;; them equal at save time and so deletes the persisted variable file.
;; To work around this, we set the default value to nil and initialize
;; it to a hash table "manually".
;; TODO: File a bug report against persist.el.
(persist-defvar hyperdrive-hyperdrives nil
                "List of known hyperdrives."
                hyperdrive-persist-location)
(unless hyperdrive-hyperdrives
  (setf hyperdrive-hyperdrives (make-hash-table :test #'equal)))

(defvar-local hyperdrive-current-entry nil
  "Entry for current buffer.")
(put 'hyperdrive-current-entry 'permanent-local t)

;;;; Links

(defvar browse-url-handlers)
(defvar thing-at-point-uri-schemes)

(defun hyperdrive-browse-url (url &rest _ignore)
  "Browse hyperdrive URL."
  (hyperdrive-open url))

(when (version<= "28.1" emacs-version)
  (require 'browse-url)
  (require 'thingatpt)

  (cl-pushnew (cons (rx bos "hyper://") #'hyperdrive-browse-url)
              browse-url-handlers :test #'equal)
  (cl-pushnew "hyper://" thing-at-point-uri-schemes :test #'equal))

;;;; Commands

(defun hyperdrive--gateway-pid ()
  "Return `hyper-gateway' process id if it's running. Otherwise, return nil."
  (let ((output
         (shell-command-to-string (concat "pgrep " hyperdrive-hyper-gateway-command))))
    (when (> (length output) 0)
      (string-to-number (string-trim output)))))

(defun hyperdrive--gateway-ready-p ()
  "Return non-nil if hyper-gateway is ready."
  (let (readyp)
    (with-local-quit
      (hyperdrive-api 'get "hyper://localhost" :noquery t
        ;; FIXME: Don't use else handler, since plz should not call it after a synchronous request
        :else (lambda (err)
                (unless (and (plz-error-curl-error err)
                             ;; "Failed to connect to host."
                             (= 7 (car (plz-error-curl-error err))))
                  ;; Status code 400 is expected when hyper-gateway is running
                  ;; See https://github.com/RangerMauve/hyper-gateway/issues/3
                  (when (= 400 (plz-response-status (plz-error-response err)))
                    (setq readyp t))))))
    readyp))

(defun hyperdrive--alias-url (alias)
  "Return URL to hyperdrive known as ALIAS, or nil if it doesn't exist.
That is, if the ALIAS has been used to create a local
hyperdrive."
  ;; TODO: Should this function go inside hyperdrive-lib.el?
  (condition-case err
      (pcase (with-local-quit
               (hyperdrive-api 'get (concat "hyper://localhost/?key=" (url-hexify-string alias))
                 :as 'response :noquery t))
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
  ;; TODO: Verify that the latest version is installed.  See: <https://github.com/RangerMauve/hyper-gateway/issues/9>.
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
      (hyperdrive-message "Already not running hyper-gateway."))))

;; TODO: Command to upload one or more files.
;; TODO: Command to download files.

(defun hyperdrive-revert-buffer (&optional _arg _noconfirm)
  "Revert `hyperdrive-mode' buffer by reloading hyperdrive contents."
  ;; TODO: [#C] Override buffer-modified check when buffer is erased.
  (hyperdrive-open (hyperdrive-entry-url hyperdrive-current-entry)))

;;;; hyperdrive-mode

(define-minor-mode hyperdrive-mode
  "Minor mode for buffers opened from hyperdrives."
  :global nil
  :interactive nil
  :group 'hyperdrive
  :lighter " hyperdrive"
  ;; :keymap (let ((map (make-sparse-keymap)))
  ;;           ;; TODO: [#C] Redo this command.
  ;;           (define-key map [remap dired-jump]  #'hyperdrive-up-directory)
  ;;           map)
  (if hyperdrive-mode
      (progn
        (setq-local revert-buffer-function #'hyperdrive-revert-buffer)
        (cl-pushnew #'hyperdrive--write-contents write-contents-functions))
    (setq-local revert-buffer-function #'revert-buffer--default
                write-contents-functions
                (remove #'hyperdrive--write-contents write-contents-functions))))

;;;###autoload
(defun hyperdrive-find-file (entry)
  "Find hyperdrive ENTRY.
Interactively, prompts for known hyperdrive and path."
  (interactive (list (hyperdrive-read-entry)))
  (hyperdrive-open (hyperdrive-entry-url entry)))

;;;###autoload
(cl-defun hyperdrive-open (url &key then recurse)
  "Open hyperdrive URL.
THEN may be a function to pass to the handler to call in the
buffer opened by the handler."
  (declare (indent defun))
  (interactive (list (read-string "Hyperdrive URL: ")))
  ;; TODO: Add `find-file'-like interface. See <https://todo.sr.ht/~ushin/ushin/16>
  ;; TODO: When possible, check whether drive is writable with a HEAD request, and set writablep in the
  ;; struct. If the hyperdrive already exists in hyperdrive-hyperdrives, there's no need to send a HEAD
  ;; request, since the value will never change. We only need to send a HEAD request when calling
  ;; `hyperdrive-open-url' on an unknown URL. Since `hyperdrive-complete-url' only returns a URL, we'll
  ;; need to parse the URL and then call `gethash' (or refactor `hyperdrive-complete-url').
  ;; See: <https://github.com/RangerMauve/hypercore-fetch/issues/60>.
  ;; TODO: What happens if the user tries to open a hyperdrive file that's already open in a buffer?
  (let* ((entry (hyperdrive-url-entry url))
         (hyperdrive (hyperdrive-entry-hyperdrive entry)))
    (hyperdrive-fill entry
      :then (lambda (entry)
              (pcase-let* (((cl-struct hyperdrive-entry type) entry)
                           ;; MAYBE: Use alist-get instead of cl-find-if.
                           (handler (or (cdr (cl-find-if (lambda (regexp)
                                                           (string-match-p regexp type))
                                                         hyperdrive-type-handlers :key #'car))
                                        #'hyperdrive-handler-default)))
                (hyperdrive-persist hyperdrive)
                (funcall handler entry :then then)))
      :else (lambda (plz-error)
              (cl-labels ((go-up
                           () (if recurse
                                  (hyperdrive-open (hyperdrive--parent url) :recurse t)
                                (pcase (prompt-to-go-up)
                                  (1 (hyperdrive-open (hyperdrive--parent url)))
                                  (`t (hyperdrive-open (hyperdrive--parent url) :recurse t)))))
                          (prompt-to-go-up
                           () (pcase-exhaustive
                                  (read-answer (format "URL not found: %S.  Try to load parent directory? " url)
                                               '(("yes" ?y "go up one level")
                                                 ("no" ?n "exit")
                                                 ("recurse" ?! "go up until directory found")))
                                ("yes" 1)
                                ("recurse" t)
                                ("no" nil))))
                (pcase-let (((cl-struct plz-error curl-error response) plz-error))
                  (if curl-error
                      (error "hyper-gateway not running.  Use \"M-x hyperdrive-start RET\" to start it")
                    (pcase (plz-response-status response)
                      (404 ;; Path not found.
                       (cond ((string-suffix-p "/" url)
                              ;; Path ends in a slash (and hyperdrive does not
                              ;; support empty directories): offer to go up the tree.
                              (go-up))
                             ;; Path does not end in a slash.
                             ((hyperdrive-writablep hyperdrive)
                              ;; Hyperdrive is writable: create a new buffer that will be saved to that path.
                              nil)
                             (t
                              ;; Hyperdrive not writable: offer to go up.
                              (go-up))))
                      (_ (hyperdrive-message "Unable to load URL %S: %S" url plz-error))))))))))

(defun hyperdrive-save-buffer (entry)
  "Save ENTRY to hyperdrive (interactively, the current buffer).
If buffer was not hyperdrive-backed, it becomes so."
  ;; TODO: Improve docstrings of `hyperdrive-save-buffer' and
  ;; `hyperdrive-write-buffer' after we've sorted out their behavior.
  (interactive
   (list (if hyperdrive-mode
             hyperdrive-current-entry
           (hyperdrive-read-entry :predicate #'hyperdrive-writablep))))
  (hyperdrive-write-buffer entry 'overwrite))

(defun hyperdrive-write-buffer (entry &optional overwritep)
  "Write current buffer to new hyperdrive ENTRY.
If file already exists and OVERWRITEP is nil, prompt the user to
overwrite."
  (interactive (list (hyperdrive-read-entry :predicate #'hyperdrive-writablep)))
  ;; FIXME: Overwrites without prompting if file exists.  Make new
  ;; --writable-p based on
  ;; <https://github.com/RangerMauve/hypercore-fetch/issues/60>.
  (ignore overwritep)
  (unless hyperdrive-mode
    ;; TODO: Remove faces/overlays that might be applied to current buffer.
    ;; FIXME: Reusing the same buffer does not remove buffer-file-name. If you call
    ;; `hyperdrive-write-buffer' from a file-visiting buffer, the buffer name as well as
    ;; `revert-buffer-function' and `write-contents-functions' will be set. Now, any attempt to
    ;; reopen the file you started with opens the renamed buffer. The only way to open the file
    ;; in a new buffer is to kill the renamed buffer (or set its `buffer-file-name' to nil).
    ;;
    ;; I tried adding (set-visited-file-name nil) inside `hyperdrive-mode-on', but that
    ;; resets `major-mode', `revert-buffer-function', and `write-contents-functions'.
    ;;
    ;; Should we be concerned about other potential permanent-local
    ;; variables besides `buffer-file-name' messing things up?

    ;; Possible fix: Just (setf buffer-file-name nil).
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
                  (rename-buffer (hyperdrive--format-entry-url entry) 'unique)
                  (set-buffer-modified-p nil)))
              (hyperdrive-message "Wrote: %S to %S" name url))
      :else (lambda (plz-error)
              (pcase-let* (((cl-struct plz-error response) plz-error)
                           ((cl-struct plz-response status body) response)
                           ;; TODO: hyper-gateway should return 403
                           ;; when not writable.  See:
                           ;; <https://todo.sr.ht/~ushin/ushin/25>.
                           (message
                            (if (and (eq 500 status)
                                     (string-match-p "SESSION_NOT_WRITABLE" body))
                                "Hyperdrive not writable"
                              plz-error)))
                (hyperdrive-message "Unable to write: %S: %S" name message))))
    (hyperdrive-message "Saving to %S..." url)
    ;; TODO: Reload relevant hyperdrive-dir buffers after writing buffer (if ewoc buffers display etag, then possibly all ewoc buffers for a given hyperdrive should be reloaded)
    ))

(defun hyperdrive--write-contents ()
  "Call `hyperdrive-save-buffer' for the current buffer.
To be used in `write-contents-functions'."
  (cl-assert hyperdrive-mode)
  (hyperdrive-save-buffer hyperdrive-current-entry))

(defun hyperdrive-copy-url (entry)
  "Save hyperdrive ENTRY's URL to the kill ring.
Interactively, uses `hyperdrive-current-entry', from either a
hyperdrive directory listing or a `hyperdrive-mode' file buffer."
  (interactive (list hyperdrive-current-entry))
  (let ((url (hyperdrive-entry-url entry)))
    (kill-new url)
    (hyperdrive-message "%s" url)))

;;;; Footer

(provide 'hyperdrive)
;;; hyperdrive.el ends here
