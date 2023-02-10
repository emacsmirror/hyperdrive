;;; hyperdrive.el --- Emacs gateway to the Hypercore network  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Joseph Turner <joseph@ushin.org>

;; Author: Joseph Turner
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

;; hyperdrive.el relies on [hyper-gateway](https://github.com/RangerMauve/hyper-gateway/) for talking to the hypercore network.
;; Download or compile the hyper-gateway (https://github.com/RangerMauve/hyper-gateway/releases) binary and ensure that it is executable and in your $PATH.

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

;; Ensure that `hyper-hyper-gateway-command' is set to the name you gave to the hyper-gateway binary.

;;; Code:

(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'rx)
  (require 'json))
(require 'plz)
(require 'mpv)
(require 'persist)

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

;;;; Internal variables

(defconst hyperdrive--hyper-prefix "hyper://" "Hyper prefix.")

(defconst hyperdrive--org-link-type "hyper" "Org mode link type.")

(defconst hyperdrive--public-key-re
  (rx (eval hyperdrive--hyper-prefix) (group (= 52 alphanumeric)))
  "Regex to match \"hyper://\" + public key.

Capture group matches public key.")

(defconst hyperdrive--version-re
  (rx (eval hyperdrive--hyper-prefix)
      (one-or-more alnum)
      (group "+" (one-or-more num)))
  "Regex to match \"hyper://\" + public key or alias + version number.

Capture group matches version number.")

(persist-defvar hyperdrive--namespaces nil
                "List of cons pairs mapping an alias to a public key."
                hyperdrive-persist-location)

(defvar hyperdrive--current-url nil
  "The url of the current hyperdrive file or directory.

Depending on how `hyperdrive-load-url' was called to generate the current buffer, the url may or may not contain a version number.")
(put 'hyperdrive--current-url 'permanent-local t)

;;;; Faces

(defgroup hyperdrive-dired-faces nil
  "Faces used by hyperdrive dired."
  :group 'faces
  :prefix "hyperdrive-dired-")

(defface hyperdrive-dired-header
  '((t (:inherit font-lock-type-face)))
  "Face used for hyperdrive directory headers."
  :group 'hyperdrive-dired-faces)

(defface hyperdrive-dired-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for hyperdrive subdirectories."
  :group 'hyperdrive-dired-faces)

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

;;;; Helper functions

(defun hyperdrive--get-public-key-by-alias (alias)
  "Get public key corresponding to ALIAS in `hyperdrive--namespaces'."
  (cdr (assoc alias hyperdrive--namespaces)))

(defun hyperdrive--convert-to-hyper-gateway-url (url)
  "Convert a URL starting with `hyperdrive--hyper-prefix' to one
 starting with \"http://localhost:4973/hyper/\" (assuming that
 4973 is the value of `hyper-gateway-port')."
  (concat "http://localhost:" (number-to-string hyperdrive-hyper-gateway-port) "/hyper/"
          (substring url (length hyperdrive--hyper-prefix))))

(defun hyperdrive--make-hyperdrive-url (public-key raw-path)
  "Generate a `hyperdrive--hyper-prefix'-prefixed URL from a
PUBLIC-KEY and PATH.

PATH is an absolute path, starting from the top-level directory
of the hyperdrive."
  (let ((path (if (string-match-p "^/" raw-path) raw-path (concat "/" raw-path))))
    (concat hyperdrive--hyper-prefix public-key path)))

(defun hyperdrive--extract-public-key (string)
  "Extract public-key from STRING using `hyperdrive--public-key-re'."
  (when (string-match hyperdrive--public-key-re string)
    (match-string 1 string)))

(defun hyperdrive--maybe-get-alias-from-public-key (url)
  "Return alias corresponding to public key in URL. Otherwise, return URL."
  (car (rassoc (hyperdrive--extract-public-key url) hyperdrive--namespaces)))

;; TODO: Handle duplicate alias/name urls, like foo:/path/to/file

(defun hyperdrive--replace-public-key-with-alias (url alias)
  "Replace public key in URL with corresponding ALIAS.

For example, if URL corresponds to alias \"foo\" according to
`hyperdrive--namespaces', replace \"hyper://<public key
for foo>/\" with \"foo:/\".

Otherwise, return URL."
  (replace-regexp-in-string
   (concat hyperdrive--hyper-prefix (hyperdrive--extract-public-key url))
   (concat alias ":")
   url))

(defun hyperdrive--extract-path (string)
  "Extract path following public-key from STRING."
  (substring string (+ (length hyperdrive--hyper-prefix)
                       (length (hyperdrive--extract-public-key string)))))

(defun hyperdrive--add-version-to-url (link version)
  "Add VERSION number to url from LINK and (optionally) VERSION.

This function returns a url of the form \"hyper://\" + public-key
+ path or \"hyper://\" + public-key + version number + path,
while urls entered by users may be namespace aliases or lack
version numbers."
  (concat hyperdrive--hyper-prefix
          (hyperdrive--extract-public-key link)
          (and version (concat "+" version))
          (hyperdrive--extract-path link)))

(defun hyperdrive--response-extract-url (response)
  "Extract url from RESPONSE.

Returned url does not contain version number."
  (let ((str (alist-get 'link (plz-response-headers response))))
    (when (string-match (rx "<" (group (one-or-more anything)) ">")
                        str)
      (match-string 1 str))))

(defun hyperdrive--response-extract-version (response)
  "Extract version number (etag) from RESPONSE.

Version number is of type string"
  (alist-get 'etag (plz-response-headers response)))

(defun hyperdrive--response-extract-contents (response directoryp)
  "Extract contents (body) from RESPONSE.

If response is a directory, read directory contents as JSON.
Otherwise, return plain buffer contents."
  (let ((json-array-type 'list)
        (contents (plz-response-body response)))
    (if directoryp
        (json-read-from-string contents)
      contents)))

(defun hyperdrive--directory-p (response)
  "Return non-nil if hyperdrive RESPONSE is a directory."
  (string-match "/$" (hyperdrive--response-extract-url response)))

(defun hyperdrive--streamable-p (url)
  "Return non-nil if hyperdrive URL points to audio or video which
can be streamed with mpv."
  (string-match
   (rx (or "audio" "video"))
   (alist-get 'content-type
              (plz-response-headers
               (plz 'head (hyperdrive--convert-to-hyper-gateway-url url)
                 :as 'response)))))

(defun hyperdrive--version-match (url)
  "Return non-nil if URL contains a version number."
  (string-match hyperdrive--version-re url))

(defun hyperdrive--get-buffer-create (url)
  "Pass URL or corresponding alias to `get-buffer-create'.

This function helps prevents duplicate `hyperdrive-mode' buffers by
ensuring that buffer names always use the namespace alias
corresponding to URL if possible.

In other words, this avoids the situation where a buffer called
\"foo:/\" and another called \"hyper://<public key for foo>/\"
both point to the same content."
  (let* ((alias (hyperdrive--maybe-get-alias-from-public-key url))
         (bufname (cond
                   (alias (hyperdrive--replace-public-key-with-alias url alias))
                   (t url))))
    (get-buffer-create bufname)))

;;;; Commands

(defun hyperdrive-public-key (alias)
  "Copy the formatted public key corresponding to ALIAS to kill-ring."
  (interactive (list (hyperdrive--completing-read-alias)))
  (let* ((public-key (hyperdrive--get-public-key-by-alias alias))
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
    (plz 'get
      (hyperdrive--convert-to-hyper-gateway-url (concat hyperdrive--hyper-prefix "localhost/?key="))
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

(defun hyperdrive-save-buffer (url)
  "Save contents of current buffer to URL.

URL must be writable."
  (interactive (list hyperdrive--current-url))
  (if url
      (plz 'put (hyperdrive--convert-to-hyper-gateway-url url)
        :body-type 'binary
        :body (buffer-substring-no-properties (point-min) (point-max)))
    (call-interactively #'hyperdrive-save-buffer-by-alias)))

(defun hyperdrive-save-buffer-by-alias (alias path)
  "Save contents of current buffer as a file at PATH in namespaced
hyperdrive corresponding to ALIAS.

PATH represents the absolute path inside the hyperdrive."
  (interactive (list
                (hyperdrive--completing-read-alias)
                (read-string "Path in hyperdrive: ")))
  (hyperdrive-save-buffer
   (hyperdrive--make-hyperdrive-url (hyperdrive--get-public-key-by-alias alias) path)))

(defun hyperdrive-upload-files (alias relative-dir files)
  "Upload files from the local filesystem to the hyperdrive for ALIAS.

RELATIVE-DIR is the directory relative to which files in the
hyperdrive will be inserted.

FILES can be either be a list of filepaths (strings) or a
function which returns a list of filepaths."
  (let ((file-list
         (if (functionp files)
             (funcall files)
           files)))
    (dolist (path file-list)
      (with-temp-buffer
        (insert-file-contents path)
        (hyperdrive-save-buffer-by-alias
         alias (concat "/" (file-relative-name path relative-dir)))))))

(defun hyperdrive-create-namespace (alias)
  "Create a hyperdrive namespace from an alphanumeric ALIAS.

This adds a new cons pair to `hyperdrive--namespaces'.

This function is idempotent; running it multiple times with the
same ALIAS does not create a new namespace."
  (interactive "sNamespace alias: ")
  (let ((public-key (hyperdrive--extract-public-key
                     (plz-response-body
                      (plz 'post (hyperdrive--convert-to-hyper-gateway-url
                                  (concat hyperdrive--hyper-prefix "localhost/?key=" alias))
                        :as 'response)))))
    (cl-pushnew (cons alias public-key) hyperdrive--namespaces :test #'equal)
    (message "%s: %s" alias (hyperdrive--make-hyperdrive-url public-key ""))))

(defun hyperdrive-load-alias (alias)
  "Load hyperdrive corresponding to ALIAS."
  (interactive (list (hyperdrive--completing-read-alias)))
  (hyperdrive-load-url (hyperdrive--make-hyperdrive-url (hyperdrive--get-public-key-by-alias alias) "")))

(defun hyperdrive-load-url (url &optional use-version cb)
  "Load contents at URL from Hypercore network.

If CB is non-nil, pass it the url and loaded contents. Otherwise,
call either `hyperdrive-dired' or `hyperdrive-find-file'.

URL should begin with `hyperdrive--hyper-prefix'.

If URL contains a version number or if USE-VERSION is non-nil,
ensure that the final url displays the version number."
  (interactive "sURL: ")
  (if (hyperdrive--streamable-p url)
      (mpv-play-url (hyperdrive--convert-to-hyper-gateway-url url))
    (plz 'get (hyperdrive--convert-to-hyper-gateway-url url)
      :as 'response
      :then (lambda (response)
              (let* ((directoryp (hyperdrive--directory-p response))
                     (contents (hyperdrive--response-extract-contents response directoryp))
                     (use-version (or use-version (hyperdrive--version-match url)))
                     (url-without-version (hyperdrive--response-extract-url response)))
                (setq url (if use-version
                              (hyperdrive--add-version-to-url url-without-version
                                                              (hyperdrive--response-extract-version response))
                            url-without-version))
                (cond (cb (funcall cb url contents directoryp))
                      (directoryp (hyperdrive-dired url contents))
                      (t (hyperdrive-find-file url contents))))))))

(defun hyperdrive-delete-file (url)
  "Delete file at URL."
  (plz 'delete (hyperdrive--convert-to-hyper-gateway-url url)
    :as 'response
    :then (lambda (_response) (hyperdrive-load-url (hyperdrive--get-parent-directory url)))
    :else (lambda (err)
            (when (= 403 (plz-response-status (plz-error-response err)))
              (user-error "Not Authorized to delete: %s" url)))))

(defun hyperdrive-find-file (url contents)
  "Switch to a buffer with CONTENTS of file at URL.

If `hyperdrive-honor-auto-mode-alist' is non-nil, set `major-mode' according to file
extension."
  (with-current-buffer (hyperdrive--get-buffer-create url)
    (setq-local hyperdrive--current-url url)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert contents)
      ;; Inspired by https://emacs.stackexchange.com/a/2555/39549
      (when hyperdrive-honor-auto-mode-alist
        (let ((buffer-file-name hyperdrive--current-url))
          (set-auto-mode))))
    (switch-to-buffer (current-buffer))
    (hyperdrive-mode)))

(defun hyperdrive-up-directory ()
  "Visit parent directory of current hyperdrive file or directory."
  (interactive)
  (hyperdrive-load-url (hyperdrive--get-parent-directory)))

(defun hyperdrive-revert-buffer (&optional _arg _noconfirm)
  "Revert `hyperdrive-mode' buffer by reloading hyperdrive contents."
  (widen)
  (hyperdrive-load-url hyperdrive--current-url))

(defun hyperdrive--get-parent-directory (&optional url)
  "Get parent directory of URL or current hyperdrive file or directory if URL is nil.

If already at top-level directory, return current directory."
  (let* ((url (or url hyperdrive--current-url))
         (parent-dir (file-name-directory (directory-file-name url))))
    (if (equal parent-dir hyperdrive--hyper-prefix)
        hyperdrive--current-url
      parent-dir)))

;;;; Org links

(defun hyperdrive-store-link ()
  "Store a link to the hyperdrive file.

In a `hyperdrive-dired' buffer, store link to file at point. In a
buffer with `hyperdrive-mode' enabled, store file corresponding
to current buffer."
  (cond
   ((derived-mode-p 'hyperdrive-dired-mode)
    (setq org-store-link-plist nil)
    (org-link-store-props :type hyperdrive--org-link-type
                          :link (hyperdrive-dired-get-filename)))
   ((bound-and-true-p hyperdrive-mode)
    (setq org-store-link-plist nil)
    (org-link-store-props :type hyperdrive--org-link-type
                          :link hyperdrive--current-url))))

(defun hyperdrive-open-link (url)
  "Open the hyperdrive URL, pulling latest updates from the network.

Call `org-*' functions to handle search option if URL contains it."
  (let* ((url-without-option (car (split-string url "::")))
         (option (cadr (split-string url "::")))
         (line (and option
                    (string-match-p "\\`[0-9]+\\'" option)
		    (list (string-to-number option))))
         (search (and (not line) option)))
    (hyperdrive-load-url (concat "hyper:" url-without-option) nil
                         (lambda (url contents directoryp)
                           (if directoryp
                               (hyperdrive-dired url contents)
                             (hyperdrive-find-file url contents)
                             (with-current-buffer (hyperdrive--get-buffer-create url)
                               (setq-local hyperdrive--current-url url)
                               (cond (line (org-goto-line line)
		                           (when (derived-mode-p 'org-mode) (org-fold-reveal)))
	                             (search (condition-case err
			                         (org-link-search search)
                                               (error (message "%s" (nth 1 err))))))))))))

(org-link-set-parameters hyperdrive--org-link-type :store #'hyperdrive-store-link :follow #'hyperdrive-open-link)

;;;; hyperdrive-mode

(defun hyperdrive-mode-on ()
  "Activate `hyperdrive-mode'."
  (setq buffer-read-only t)
  (setq-local revert-buffer-function #'hyperdrive-revert-buffer))

(defun hyperdrive-mode-off ()
  "Deactivate `hyperdrive-mode'."
  (setq buffer-read-only nil))

(define-minor-mode hyperdrive-mode
  "Make buffer read-only and set `revert-buffer-function'."
  :global nil
  :group 'hyperdrive
  :lighter "hyperdrive"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map [remap dired-jump]  #'hyperdrive-up-directory)
            (define-key map (kbd "C-x C-s") #'hyperdrive-save-buffer)
            ;; FIXME: The following doesn't work?
            ;; (define-key map [remap save-buffer] #'hyperdrive-save-buffer)
            map)
  (if hyperdrive-mode
      (hyperdrive-mode-on)
    (hyperdrive-mode-off)))

;;;; hyperdrive-dired

(defun hyperdrive-dired (url contents)
  "Switch to a buffer with CONTENTS of directory at URL."
  (with-current-buffer (hyperdrive--get-buffer-create url)
    (setq-local hyperdrive--current-url url)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (hyperdrive-dired-insert-directory-contents url contents))
    (hyperdrive-dired-mode)
    (switch-to-buffer (current-buffer))
    (goto-line 4)))

(defun hyperdrive-dired-insert-directory-contents (url contents)
  "Display hyperdrive directory CONTENTS for URL in a Dired-like interface."
  (insert "  " (propertize url  'face 'hyperdrive-dired-header) ":" "\n"
          "  " (propertize "."  'face 'hyperdrive-dired-directory)  "\n"
          "  " (propertize ".." 'face 'hyperdrive-dired-directory)  "\n")
  (when (equal (car contents) "$/") ;; Remove special top-level directory: https://github.com/RangerMauve/hypercore-fetch/issues/32
    (setq contents (cdr contents)))
  (dolist (item (sort contents #'string<))
    (if (string-match-p "/$" item)
        (insert "  " (propertize item 'face 'hyperdrive-dired-directory) "\n")
      (insert "  " item "\n"))))

(defun hyperdrive-dired-find-file ()
  "In `hyperdrive-dired-mode', visit the file or directory named on this line."
  (interactive)
  (hyperdrive-load-url (hyperdrive-dired-get-filename)))

(defun hyperdrive-dired-get-filename ()
  "Get the current line's file name, with an error if file does not exist."
  (interactive)
  (let ((raw)
        (filename))
    (if (= 1 (line-number-at-pos))
        hyperdrive--current-url
      (setq raw (string-trim (thing-at-point 'line t)))
      (setq filename
            (cond ((equal "."  raw) hyperdrive--current-url)
                  ((equal ".." raw) (hyperdrive--get-parent-directory))
                  (t (concat hyperdrive--current-url raw)))))))

(defun hyperdrive-dired-copy-filename-as-kill ()
  "Copy hyperdrive url of file at point."
  (interactive)
  (let ((string (hyperdrive-dired-get-filename)))
    (kill-new string)
    (message "%s" string)))

(defun hyperdrive-dired-delete-file ()
  "Delete file on current line with confirmation."
  (interactive)
  (when (y-or-n-p "Delete file? ")
    (hyperdrive-delete-file (hyperdrive-dired-get-filename))))

(defvar-keymap hyperdrive-dired-mode-map
  :parent  special-mode-map
  :doc "Local keymap for `hyperdrive-dired-mode' buffers."
  "RET"     #'hyperdrive-dired-find-file
  "^"       #'hyperdrive-up-directory
  "w"       #'hyperdrive-dired-copy-filename-as-kill
  "D"       #'hyperdrive-dired-delete-file)

(define-derived-mode hyperdrive-dired-mode special-mode "hyperdrive-dired"
  "Major mode for viewing hyperdrive directories.
\\{hyperdrive-dired-mode-map}"
  (hyperdrive-mode +1))

(provide 'hyperdrive)
;;; hyperdrive.el ends here
