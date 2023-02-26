;;; hyperdrive.el --- Emacs gateway to the Hypercore network  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joseph Turner <joseph@ushin.org>

;; Author: Joseph Turner
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <joseph@ushin.org>
;; Created: 2022
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3") (compat "29.1.3.2") (plz "0.3") (mpv "0.2.0") (persist "0.5"))
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
(require 'map)
(require 'rx)

(require 'plz)
(require 'mpv)
(require 'persist)

(require 'hyperdrive-lib)
(require 'hyperdrive-handlers)

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

(defcustom hyperdrive-persist-location (locate-user-emacs-file "var/hyperdrive")
  "Location where `persist' will store data."
  :type 'sexp)

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

;;;; Internal variables

(persist-defvar hyperdrive--namespaces nil
                "List of cons pairs mapping an alias to a public key."
                hyperdrive-persist-location)

(defconst hyperdrive--org-link-type "hyper" "Org mode link type.")

(defvar-local hyperdrive-current-entry nil
  "Entry for current buffer.")
(put 'hyperdrive-current-entry 'permanent-local t)

;;;; User interaction helper functions

(defun hyperdrive--completing-read-alias ()
  "Return an alias from `hyperdrive--namespaces'.

Prompt user to select an alias, or if only one namespace exists,
select it automatically."
  (if hyperdrive--namespaces
      (if (= 1 (length hyperdrive--namespaces))
          (caar hyperdrive--namespaces)
        (completing-read "Alias: " hyperdrive--namespaces nil t))
    (user-error "No namespace defined. Please run M-x hyperdrive-create-namespace"))) ; TODO: Prompt user to create namespace here?

;;;; Commands

(defun hyperdrive-public-key (alias)
  "Copy the formatted public key corresponding to ALIAS to kill-ring."
  (interactive (list (hyperdrive--completing-read-alias)))
  (let* ((public-key (hyperdrive--public-key-by-alias alias))
         (formatted-key (hyperdrive--make-hyperdrive-url public-key "")))
    (message "%s" formatted-key)
    (kill-new formatted-key)))

(defun hyperdrive--gateway-pid ()
  "Return `hyper-gateway' process id if it's running. Otherwise, return nil."
  (let ((output
         (shell-command-to-string (concat "pgrep " hyperdrive-hyper-gateway-command))))
    (when (> (length output) 0)
      (string-to-number (string-trim output)))))

(defun hyperdrive--gateway-ready-p ()
  "Return non-nil if hyper-gateway is ready."
  (let (readyp)
    (hyperdrive-api 'get (concat hyperdrive--hyper-prefix "localhost/?key=")
      :else (lambda (err)
              (unless (and (plz-error-curl-error err)
                           ;; "Failed to connect to host."
                           (= 7 (car (plz-error-curl-error err))))
                ;; Status code 400 is expected when hyper-gateway is running
                ;; See https://github.com/RangerMauve/hyper-gateway/issues/3
                (when (= 400 (plz-response-status (plz-error-response err)))
                  (setq readyp t)))))
    readyp))

;;;###autoload
(defun hyperdrive-start-gateway ()
  "Start `hyper-gateway' if not already running."
  (interactive)
  (unless (hyperdrive--gateway-ready-p)
    (let ((buf (get-buffer-create "hyper-gateway")))
      (with-current-buffer buf (erase-buffer))
      (make-process
       :name "hyper-gateway"
       :buffer buf
       :command (list hyperdrive-hyper-gateway-command "--writable" "true" "run")))))

(defun hyperdrive-stop-gateway ()
  "Stop the `hyper-gateway' process."
  (interactive)
  (let ((proc (hyperdrive--gateway-pid)))
    (if proc
        (signal-process proc 'sigint)
      (message "Already not running hyper-gateway."))))

;; (defun hyperdrive-save-buffer (url)
;;   "Save contents of current buffer to URL.

;; URL must be writable and not a directory."
;;   (interactive (list hyperdrive--current-url))
;;   (if (and url (not (hyperdrive--directory-p url)))
;;       (hyperdrive-api 'put url
;;                       :body-type 'binary
;;                       :body (buffer-substring-no-properties (point-min) (point-max)))
;;     (call-interactively #'hyperdrive-save-buffer-by-alias)))

(defun hyperdrive--save-buffer ()
  "Save current buffer to its hyperdrive location.
Only for `hyperdrive-mode' file buffers."
  ;; TODO: Rename to hyperdrive--write-buffer ?
  (cl-assert hyperdrive-current-entry)
  (pcase-let (((cl-struct hyperdrive-entry url) hyperdrive-current-entry))
    (hyperdrive-api 'put url
      :body-type 'binary
      :body (buffer-substring (point-min) (point-max)))
    (hyperdrive-message "Saved to: %s" url)))

(defun hyperdrive-write-buffer ()
  "Write current buffer to a hyperdrive file.
The buffer may already be a hyperdrive file, or it may not be."
  (interactive)
  (pcase-let* ((filename (buffer-file-name))
               (basename (when filename
                           (file-name-nondirectory filename)))
               (alias (hyperdrive--completing-read-alias))
               (public-key (hyperdrive--public-key-by-alias alias))
               (name (read-string "Filename: " nil nil basename))
               (hyperdrive-current-entry
                (make-hyperdrive-entry
                 :name name
                 :url (hyperdrive--make-hyperdrive-url public-key name))))
    (hyperdrive--save-buffer)))

;; (defun hyperdrive-save-buffer-by-alias (alias path)
;;   "Save contents of current buffer as a file at PATH in namespaced
;; hyperdrive corresponding to ALIAS.

;; PATH represents the absolute path inside the hyperdrive."
;;   (interactive (list
;;                 (hyperdrive--completing-read-alias)
;;                 (read-string "Path in hyperdrive: ")))
;;   (hyperdrive-save-buffer
;;    (hyperdrive--make-hyperdrive-url (hyperdrive--public-key-by-alias alias) path)))

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

;; (defun hyperdrive-delete-file (url)
;;   "Delete file at URL.

;; Note that deleted files can be accessed by checking out a prior
;; version of the hyperdrive."
;;   (hyperdrive-api 'delete url)
;;   (hyperdrive-up-directory url)
;;   (message "Deleted files can be accessed by checking out a prior version of the hyperdrive."))

(defun hyperdrive-revert-buffer (&optional _arg _noconfirm)
  "Revert `hyperdrive-mode' buffer by reloading hyperdrive contents."
  (hyperdrive-open (hyperdrive-entry-url hyperdrive-current-entry)))

;;;; Org links

;; TODO: Update Org link functions.

;; (defun hyperdrive-store-link ()
;;   "Store a link to the hyperdrive file.

;; In a `hyperdrive-dired' buffer, store link to file at point. In a
;; buffer with `hyperdrive-mode' enabled, store file corresponding
;; to current buffer."
;;   (cond
;;    ((derived-mode-p 'hyperdrive-dired-mode)
;;     (setq org-store-link-plist nil)
;;     (org-link-store-props :type hyperdrive--org-link-type
;;                           :link (hyperdrive-dired-get-filename)))
;;    ((bound-and-true-p hyperdrive-mode)
;;     (setq org-store-link-plist nil)
;;     (org-link-store-props :type hyperdrive--org-link-type
;;                           :link hyperdrive--current-url))))

;; (defun hyperdrive-open-link (url)
;;   "Open the hyperdrive URL, pulling latest updates from the network.

;; Call `org-*' functions to handle search option if URL contains it."
;;   (let* ((url-without-option (car (split-string url "::")))
;;          (option (cadr (split-string url "::")))
;;          (line (and option
;;                     (string-match-p "\\`[0-9]+\\'" option)
;; 		    (list (string-to-number option))))
;;          (search (and (not line) option)))
;;     (hyperdrive-load-url (concat "hyper:" url-without-option))
;;     (when option
;;       (with-current-buffer (hyperdrive--get-buffer-create url)
;;         (cond (line (org-goto-line line)
;; 		    (when (derived-mode-p 'org-mode) (org-fold-reveal)))
;; 	      (search (condition-case err
;; 			  (org-link-search search)
;;                         (error "%s" (nth 1 err)))))))))

;; (org-link-set-parameters hyperdrive--org-link-type :store #'hyperdrive-store-link :follow #'hyperdrive-open-link)

;;;; hyperdrive-mode

(defun hyperdrive-mode-on ()
  "Activate `hyperdrive-mode'."
  (setq-local revert-buffer-function #'hyperdrive-revert-buffer)
  (cl-pushnew #'hyperdrive--save-buffer write-contents-functions))

(defun hyperdrive-mode-off ()
  "Deactivate `hyperdrive-mode'."
  (setq-local revert-buffer-function #'revert-buffer--default
              write-contents-functions (remove #'hyperdrive--save-buffer write-contents-functions)))

(define-minor-mode hyperdrive-mode
  "Minor mode for buffers opened from hyperdrives."
  :global nil
  :group 'hyperdrive
  :lighter "hyperdrive"
  ;; :keymap (let ((map (make-sparse-keymap)))
  ;;           ;; TODO: Redo this command.
  ;;           (define-key map [remap dired-jump]  #'hyperdrive-up-directory)
  ;;           map)
  (if hyperdrive-mode
      (hyperdrive-mode-on)
    (hyperdrive-mode-off)))

;;;; hyperdrive-dired

;; (defun hyperdrive-dired-insert-directory-contents (url contents)
;;   "Display hyperdrive directory CONTENTS for URL in a Dired-like interface."
;;   (insert "  " (propertize url  'face 'hyperdrive-dired-header) ":" "\n"
;;           "  " (propertize "."  'face 'hyperdrive-dired-directory)  "\n"
;;           "  " (propertize ".." 'face 'hyperdrive-dired-directory)  "\n")
;;   (when (equal (car contents) "$/") ;; Remove special top-level directory: https://github.com/RangerMauve/hypercore-fetch/issues/32
;;     (setq contents (cdr contents)))
;;   (dolist (item (sort contents #'string<))
;;     (if (string-match-p "/$" item)
;;         (insert "  " (propertize item 'face 'hyperdrive-dired-directory) "\n")
;;       (insert "  " item "\n"))))

;; (defun hyperdrive-dired-find-file ()
;;   "In `hyperdrive-dired-mode', visit the file or directory named on this line."
;;   (interactive)
;;   (hyperdrive-load-url (hyperdrive-dired-get-filename)))

;; (defun hyperdrive-dired-get-filename ()
;;   "Get the current line's file name, with an error if file does not exist."
;;   (interactive)
;;   (let ((raw)
;;         (filename))
;;     (if (= 1 (line-number-at-pos))
;;         hyperdrive--current-url
;;       (setq raw (string-trim (thing-at-point 'line t)))
;;       (setq filename
;;             (cond ((equal "."  raw) hyperdrive--current-url)
;;                   ((equal ".." raw) (hyperdrive--parent-directory))
;;                   (t (concat hyperdrive--current-url raw)))))))

;; (defun hyperdrive-dired-copy-filename-as-kill ()
;;   "Copy hyperdrive url of file at point."
;;   (interactive)
;;   (let ((string (hyperdrive-dired-get-filename)))
;;     (kill-new string)
;;     (message "%s" string)))

;; (defun hyperdrive-dired-delete-file ()
;;   "Delete file on current line with confirmation."
;;   (interactive)
;;   (when (y-or-n-p "Delete file? ")
;;     (hyperdrive-delete-file (hyperdrive-dired-get-filename))))

;; (defvar-keymap hyperdrive-dired-mode-map
;;   :parent  special-mode-map
;;   :doc "Local keymap for `hyperdrive-dired-mode' buffers."
;;   "RET"     #'hyperdrive-dired-find-file
;;   "^"       #'hyperdrive-up-directory
;;   "w"       #'hyperdrive-dired-copy-filename-as-kill
;;   "D"       #'hyperdrive-dired-delete-file)

;; (define-derived-mode hyperdrive-dired-mode special-mode "hyperdrive-dired"
;;   "Major mode for viewing hyperdrive directories.
;; \\{hyperdrive-dired-mode-map}"
;;   (hyperdrive-mode +1))

;;;###autoload
(defun hyperdrive-open (url)
  "Open hyperdrive URL."
  (interactive (list (read-string "URL: ")))
  ;; TODO: Ensure gateway is running.
  (let ((entry (make-hyperdrive-entry :url url)))
    (hyperdrive-fill entry
      :then (lambda (entry)
              (pcase-let* (((cl-struct hyperdrive-entry type) entry)
                           ;; MAYBE: Use alist-get instead of cl-find-if.
                           (handler (or (cdr (cl-find-if (lambda (regexp)
                                                           (string-match-p regexp type))
                                                         hyperdrive-type-handlers :key #'car))
                                        #'hyperdrive-handler-default)))
                (funcall handler entry)))
      :else (lambda (plz-error)
              (pcase-let* (((cl-struct plz-error response) plz-error)
                           ((cl-struct plz-response status) response))
                (pcase status
                  (404 (when (yes-or-no-p (format "URL not found: %S.  Try to load parent directory? " url))
                         (hyperdrive-open (hyperdrive--parent-url entry))) )
                  (_ (hyperdrive-message "Unable to load URL %S: %S" plz-error))))))))

;;;; API

(provide 'hyperdrive)
;;; hyperdrive.el ends here
