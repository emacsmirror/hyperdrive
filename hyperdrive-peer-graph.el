;;; hyperdrive-peer-graph.el --- View graph of sources, blockers, and blocked  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>
;; Maintainer: Joseph Turner <~ushin/ushin@lists.sr.ht>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file adds a visualizer for sources, blockers, and blocked.

;;; Code:

(require 'dom)

(require 'hyperdrive-sbb-view)
(require 'hyperdrive-lib)
(require 'taxy-magit-section)

;;;; Variables:

(defgroup hyperdrive-peer-graph nil
  "Customization of the hyperdrive peer graph."
  :group 'hyperdrive)

(defcustom hpg/sources-max-hops-default 3
  "Default number of hops to jump for sources."
  :type 'integer)

(defcustom hpg/blockers-max-hops-default 3
  "Default number of hops to jump for blockers."
  :type 'integer)

(defcustom hpg/show-sources-p-default t
  "Default setting to show sources."
  :type 'boolean)

(defcustom hpg/show-blockers-p-default t
  "Default setting to show blockers."
  :type 'boolean)

(defcustom hpg/show-blocked-p-default 'sources
  "Default setting to show blocked."
  :type '(choice
          (const :tag "Show blocked which are also sources" sources)
          (const :tag "Show blocked which are not sources" non-sources)
          (const :tag "Show all blocked" all)
          (const :tag "Hide blocked" nil)))

(defcustom hpg/shortest-paths-p-default t
  "Default setting to filter only to shortest paths."
  :type 'boolean)

(defcustom hpg/buffer-name "*hyperdrive-peer-graph*"
  "Buffer name to show peer graph."
  :type 'string)

(defcustom hpg/list-buffer-name "*hyperdrive-peer-graph-list*"
  "Buffer name to show peer graph list."
  :type 'string)

(defcustom hpg/list-apply-filters t
  "Whether to apply `hyperdrive-peer-graph-filter' to list view."
  :type 'boolean)

(defcustom hpg/display-buffer-action '(display-buffer-reuse-window)
  "Display buffer action for hyperdrive peer graph.
Passed to `display-buffer', which see."
  :type display-buffer--action-custom-type)

(defcustom hpg/list-display-buffer-action '(display-buffer-reuse-window)
  "Display buffer action for hyperdrive peer list.
Passed to `display-buffer', which see."
  :type display-buffer--action-custom-type)

(defconst hpg/data-filename "/.well-known/peer-graph.json"
  "Hyperdrive filename to search for peer graph data.")

(defvar hpg/root-hyperdrive nil)
(defvar hpg/relations nil)

(defvar hpg/sources-max-hops hpg/sources-max-hops-default)
(defvar hpg/blockers-max-hops hpg/blockers-max-hops-default)

(defvar hpg/show-sources-p hpg/show-sources-p-default)
(defvar hpg/show-blockers-p hpg/show-blockers-p-default)
(defvar hpg/show-blocked-p hpg/show-blocked-p-default)

(defvar hpg/shortest-paths-p hpg/shortest-paths-p-default)
(defvar hpg/paths-only-to nil)

;;;; Functions:

(defvar hpg/data-cache (make-hash-table :test 'equal)
  "Hash table mapping public keys to JSON hops.")

(cl-defun hpg/data (hyperdrive &key then else)
  "Return peer graph data for HYPERDRIVE then call THEN.
THEN will be called with one argument, the parsed JSON object.

If THEN is nil or \\+`sync', the request will be synchronous and
ELSE will be ignored.  Otherwise, in case of error, ELSE will be
called with a `plz-error' struct.

If data for HYPERDRIVE is already in
`hyperdrive-peer-graph-data-cache', use it and send no request."
  (declare (indent defun))
  ;; TODO: Add a queue limit.
  (when-let ((data (gethash (h/public-key hyperdrive) hpg/data-cache)))
    ;; TODO: The first time a drive is requested, only request and parse once.
    (cl-return-from hpg/data
      (pcase then
        ((or 'nil 'sync) data)
        (_ (run-at-time 0 nil (lambda () (funcall then data)))))))
  (let ((commands `((query ((gte . ("ushin" "peers"))
                            (lt . ("ushin" "peers" "\uffff")))))))
    (cl-labels
        ((entries (json)
           (let (data)
             (mapc (pcase-lambda
                     ((map ('key (rx "ushin" 0 "peers" 0
                                     (let public-key (= 52 alphanumeric))
                                     0 (let type (+ alphanumeric))))))
                     (when public-key
                       (pcase-exhaustive type
                         ("source" (push public-key (map-elt data 'sources)))
                         ("blocker" (push public-key (map-elt data 'blockers)))
                         ("blocked" (push public-key (map-elt data 'blocked))))))
                   (cdaddr (aref json 0)))
             (puthash (h/public-key hyperdrive) data hpg/data-cache)
             data)))
      (pcase then
        ((or 'nil 'sync)
         (condition-case err
             (entries (h//bee-exec hyperdrive commands :then 'sync))
           (error
            ;; Despite error, call THEN so `pending-relations' gets decremented.
            (let ((inhibit-message t))
              (h/message "Error getting peer graph data for hyperdrive %s: %S"
                         (h/url hyperdrive) err)
              (funcall then nil)))))

        (_
         (h//bee-exec hyperdrive commands
           :then (lambda (json) (funcall then (entries json)))
           :else else))))))

(cl-defun hpg/set-relation (&key from to type bool)
  "Mark TO hyperdrive as TYPE from FROM according to BOOL.
Interactively, with universal prefix argument
\\[universal-argument], remove direct relation."
  (interactive (hpg/set-relation-interactive-args))
  ;; TODO: Consider storing source, blocker, and blocked data inside
  ;; ("ushin" "peers" ,(h/public-key to)) key to save hyperbee space.
  ;; The downside is that it would require a specialized cas function.
  (let ((commands `((put ("ushin" "peers" ,(h/public-key to) ,type) t))))
    (if bool
        (pcase type
          ;; `source' and `blocked' are mutually exclusive.
          ('source (push `(del ("ushin" "peers" ,(h/public-key to) blocked) t)
                         commands))
          ('blocked (push `(del ("ushin" "peers" ,(h/public-key to) source) t)
                          commands)))
      (setf (caar commands) 'del))
    (condition-case err
        (h//bee-exec from commands)
      (error (h/error "Unable to %s relation from %s to %s as %s: %S"
                      (if bool "set" "unset") (h/url from) (h/url to) type
                      (error-message-string err)))))
  (hpg/revert-buffers))

(cl-defun hpg/set-relation-interactive-args (&key from to type bool)
  "Return argument list for `hyperdrive-peer-graph-set-relation'.
If FROM, candidates for TO will not include FROM, and vice versa."
  (let* ((default-hyperdrive (or hpg/root-hyperdrive
                                 (and h/current-entry
                                      (he/hyperdrive h/current-entry))))
         (from (or from
                   (h/read-hyperdrive
                     :predicate
                     (if to
                         (lambda (hyperdrive)
                           (and (h/writablep hyperdrive)
                                (not (h/equal-p to hyperdrive))))
                       #'h/writablep)
                     :default
                     (and (not (and to (h/equal-p default-hyperdrive to)))
                          default-hyperdrive)
                     :prompt (if to
                                 (format "To `%s' from"
                                         (hyperdrive--format-preferred to))
                               "From"))))
         (to (or to
                 (h/read-hyperdrive
                   :predicate (lambda (hyperdrive)
                                (not (h/equal-p hyperdrive from)))
                   :default
                   (and (not (and from (h/equal-p default-hyperdrive from)))
                        default-hyperdrive)
                   :prompt (format "From `%s' to"
                                   (hyperdrive--format-preferred from)))))
         (type (or type (hpg/read-relation-type
                         (format "From `%s' to `%s', type"
                                 (hyperdrive--format-preferred from)
                                 (hyperdrive--format-preferred to)))))
         (bool (or bool (not current-prefix-arg))))
    (list :from from :to to :type type :bool bool)))

(defun hpg/hops-fn-else (then plz-error)
  "Handle PLZ-ERROR by saying demoted error and calling THEN."
  ;; Call THEN to decrement the `pending-relations' counter in `sbb-relations'.
  (pcase (plz-response-status (plz-error-response plz-error))
    ;; FIXME: If plz-error is a curl-error, this block will fail.
    (404 nil)
    (_ (h/message "Error getting peer graph data: %S" plz-error)))
  (funcall then nil))

(defun hpg/sources-hops-fn (from then)
  "Asynchronously get source hops from FROM.
Call THEN with a list of TOs."
  (hpg/data (h/create :public-key from)
    :then (lambda (data) (funcall then (map-elt data 'sources)))
    :else (apply-partially #'hpg/hops-fn-else then)))

(defun hpg/blockers-hops-fn (from then)
  "Asynchronously get blocker hops from FROM.
Call THEN with a list of TOs."
  (hpg/data (h/create :public-key from)
    :then (lambda (data) (funcall then (map-elt data 'blockers)))
    :else (apply-partially #'hpg/hops-fn-else then)))

(cl-defun hpg/blocked-hops-fn (blocker then)
  "Asynchronously get blocks from BLOCKER.
Call THEN with a list of block IDs."
  (hpg/data (h/create :public-key blocker)
    :then (lambda (data) (funcall then (map-elt data 'blocked)))
    :else (apply-partially #'hpg/hops-fn-else then)))

;; TODO: The control flow between `hyperdrive-peer-graph' is messy.
;; We need an Elisp library to abstract away the graphviz data model.

(cl-defun hpg/edge-url-format (hop type)
  "Return edge URL string for HOP of TYPE."
  (pcase-let*
      (((cl-struct h/sbb-hop from to) hop)
       (from-formatted (h//format-preferred (h/url-hyperdrive from)))
       (to-formatted (h//format-preferred (h/url-hyperdrive to))))
    (pcase type
      ('sources (format "%s\nincludes\n%s\nas a source"
                        from-formatted to-formatted))
      ('blockers (format "%s\nincludes\n%s\nas a blocker"
                         from-formatted to-formatted))
      ('blocked (format "%s\nblocks\n%s\n"
                        from-formatted to-formatted)))))

(cl-defun hpg/insert-relation (public-key relations root)
  "Insert display string for PUBLIC-KEY in current buffer.
RELATION may be a hash table of `sbb-relation' structs mapped by
\\+`public-key'.  ROOT is the \\+`public-key' of the root node."
  (pcase-let*
      ((hyperdrive (h/url-hyperdrive public-key))
       (rootp (equal public-key root))
       ((or (guard rootp)
            (cl-struct
             h/sbb-relation blocker-paths blocked-paths source-paths))
        (gethash public-key relations))
       ;; Display the value with no prefix.
       (h/formats '((petname . "%s")
                    (nickname . "%s")
                    (public-key . "%s")
                    ;; FIXME: Don't hardcode short key length.
                    (short-key . "%.8s…")
                    (seed . "%s")
                    (domains . "%s")))
       (focusp (cl-member (h/url-hyperdrive public-key)
                          hpg/paths-only-to :test #'h/equal-p))
       (label
        `(table ((border . ,(if focusp "4" "1"))
                 (cellborder . "0")
                 (color . ,(face-attribute 'default :foreground)))
                (tr nil
                    ;; TODO: Insert user color here.
                    (td ((bgcolor . ,(face-attribute 'default :background)))
                        ,(h//format-preferred hyperdrive) (br)
                        (font ((point-size . "10"))
                              ,(format "%s"
                                       (h//preferred-format hyperdrive))))))))
    (if rootp
        (dom-append-child label `(tr nil (td nil (b nil "root"))))
      (when source-paths
        (dom-append-child
         label `(tr nil (td nil (font ((color . ,h/sbb-view-sources-color))
                                      "source")))))
      (when blocker-paths
        (dom-append-child
         label `(tr nil (td nil (font ((color . ,h/sbb-view-blockers-color))
                                      "blocker")))))
      (when blocked-paths
        (dom-append-child
         label `(tr nil (td nil (font ((color . ,h/sbb-view-blocked-color))
                                      "blocked"))))))
    (insert (format "%s [label=<\n  " public-key))
    (dom-print label)
    (insert (format "\n>, href=\"%s\", color=\"%s\", bgcolor=\"%s\", shape=\"none\", margin=\"0\", style=\"filled\"];\n" public-key (face-attribute 'default :background)  (face-attribute 'default :background)))))

(cl-defun hpg/relations (root &key finally sources-max-hops blockers-max-hops)
  "Load relations from ROOT and call FINALLY.
FINALLY should be a function which accepts a single argument, a
hash table of `sbb-relation' structs keyed by public key.
SOURCES-MAX-HOPS and BLOCKERS-MAX-HOPS are the maximum number of
hops to traverse for sources and blockers, respectively."
  (h/sbb-relations root
    :hops-fn #'hpg/blockers-hops-fn :type 'blockers
    :max-hops blockers-max-hops :finally
    (lambda (relations)
      (h/sbb-blocked root relations
        :hops-fn #'hpg/blocked-hops-fn :finally
        (lambda (relations)
          (when-let* ((root-public-key (h/public-key hpg/root-hyperdrive))
                      (relation-to-root (gethash root-public-key relations))
                      (blocked-paths-to-root
                       (h/sbb-relation-paths-of-type 'blocked relation-to-root)))
            (dolist (path blocked-paths-to-root)
              (h/message
               "Ignoring blocked path to root %s from %s"
               (h/url hpg/root-hyperdrive)
               (h/url (h/url-hyperdrive (h/sbb-blocked-path-blocker path))))
              (remhash root-public-key relations)))
          (h/sbb-relations root
            :relations relations :type 'sources :max-hops sources-max-hops
            :hops-fn #'hpg/sources-hops-fn :finally finally))))))

(defun hpg/filter (relations)
  "Return filtered RELATIONS."
  ;; TODO: Make filters customizable
  ;; Apply shortest path filter first.
  (when hpg/shortest-paths-p
    (cl-callf h/sbb-filter-shortest-paths relations
      (h/public-key hpg/root-hyperdrive)))
  (unless (and hpg/show-sources-p
               hpg/show-blockers-p
               (eq hpg/show-blocked-p 'all))
    (cl-callf h/sbb-filter-to-types relations
      :sourcesp hpg/show-sources-p
      :blockersp hpg/show-blockers-p
      :blocked-sources-p (pcase hpg/show-blocked-p
                           ((or 'sources 'all) t))
      :blocked-non-sources-p (pcase hpg/show-blocked-p
                               ((or 'non-sources 'all) t))))
  ;; Apply `hpg/paths-only-to' last
  (cl-callf2 h/sbb-filter-paths-only-to (mapcar #'h/public-key hpg/paths-only-to)
             relations)
  relations)

;;;;; Reading user input

(cl-defun hpg/read-relation-type (&optional (prompt "Relation type"))
  "Return one of \\+`source', \\+`blocker', or \\+`blocked'.
Prompt with PROMPT if non-nil."
  (let ((string (completing-read
                 (format-prompt prompt "source")
                 '(source blocker blocked) nil t nil nil "source")))
    (pcase string
      ("source" 'source)
      ("blocker" 'blocker)
      ("blocked" 'blocked))))

(cl-defun hpg/context-root-hyperdrive (&key force-prompt)
  "Return `hyperdrive-peer-graph-root-hyperdrive' or prompt for drive.
With FORCE-PROMPT, or interactively with universal prefix
argument \\[universal-argument], always prompt."
  (if (and hpg/root-hyperdrive (not force-prompt))
      hpg/root-hyperdrive
    (h/read-hyperdrive :default (or (and h/current-entry
                                         (he/hyperdrive h/current-entry))
                                    hpg/root-hyperdrive))))

(defvar hpg/max-hops-history nil
  "Minibuffer history of `hyperdrive-peer-graph-read-max-hops'.")

(defun hpg/read-max-hops (type)
  "Read max hops for \\+`sources' or \\+`blockers' TYPE."
  (let ((default (pcase type
                   ('sources hpg/sources-max-hops)
                   ('blockers hpg/blockers-max-hops))))
    (read-number (format "Max hops for %s: " type) default
                 hpg/max-hops-history)))

(defun hpg/context-max-hops (type &key force-prompt)
  "Return `hyperdrive-peer-graph-sources-max-hops' or prompt.
With FORCE-PROMPT, or interactively with universal prefix
argument \\[universal-argument], always prompt."
  (if force-prompt
      (hpg/read-max-hops type)
    (pcase type
      ('sources hpg/sources-max-hops)
      ('blockers hpg/blockers-max-hops))))

(defun hpg/interactive-args ()
  "Return list of interactive args for `hyperdrive-peer-graph'."
  (let* ((root-hyperdrive (hpg/context-root-hyperdrive
                           :force-prompt current-prefix-arg))
         (root-changed (not (eq root-hyperdrive hpg/root-hyperdrive)))
         (sources-max-hops (hpg/context-max-hops
                            'sources :force-prompt (or current-prefix-arg
                                                       root-changed)))
         (blockers-max-hops (hpg/context-max-hops
                             'blockers
                             :force-prompt (or current-prefix-arg
                                               root-changed))))
    (list root-hyperdrive sources-max-hops blockers-max-hops)))

;;;; Peer Graph

(defun hyperdrive-peer-graph (hyperdrive sources-max-hops blockers-max-hops)
  "Show menu for HYPERDRIVE peer graph."
  (interactive (hpg/interactive-args))
  (cond ((hpg/need-refresh-p hyperdrive sources-max-hops blockers-max-hops)
         (setf hpg/root-hyperdrive hyperdrive)
         (setf hpg/sources-max-hops sources-max-hops)
         (setf hpg/blockers-max-hops blockers-max-hops)
         (hpg/revert-buffers))
        ((not (buffer-live-p (get-buffer hpg/buffer-name)))
         (hpg/draw-graph)))
  (pop-to-buffer hpg/buffer-name hpg/display-buffer-action))

(defun hpg/need-refresh-p (hyperdrive sources-max-hops blockers-max-hops)
  "Return non-nil if the graph data parameters have changed."
  (not (and hpg/root-hyperdrive
            (h/equal-p hyperdrive hpg/root-hyperdrive)
            (equal sources-max-hops hpg/sources-max-hops)
            (equal blockers-max-hops hpg/blockers-max-hops)
            (hpg/loaded-relations))))

(cl-defun hpg/load (&key finally)
  "Load `hpg/relations' and call FINALLY with no arguments."
  ;; TODO: If called in rapid succession, stop the requests from the first call.
  (setf hpg/relations
        (hpg/relations
         (h/public-key hpg/root-hyperdrive)
         :sources-max-hops hpg/sources-max-hops
         :blockers-max-hops hpg/blockers-max-hops
         :finally
         (lambda (relations)
           (setf hpg/relations relations)
           (h/fill-metadata-all
            (cons hpg/root-hyperdrive
                  (mapcar #'h/url-hyperdrive (hash-table-keys hpg/relations)))
            :finally finally)))))

(defun hpg/get-buffer-create ()
  "Return hyperdrive peer graph buffer with mode enabled."
  (with-current-buffer (get-buffer-create hpg/buffer-name)
    (hpg/mode)
    (current-buffer)))

(defun hpg/draw-loading-buffer ()
  "Draw loading buffer for hyperdrive peer graph."
  (with-current-buffer (hpg/get-buffer-create)
    (with-silent-modifications
      (erase-buffer)
      (insert "Loading hyperdrive peer graph data..."))))

(defun hpg/draw-graph ()
  "Draw buffer displaying hyperdrive peer graph."
  (with-current-buffer (hpg/get-buffer-create)
    (h/sbb-view (hpg/filter hpg/relations)
                 (h/public-key hpg/root-hyperdrive)
                 :insert-relation-fun #'hpg/insert-relation
                 :edge-url-format-fun #'hpg/edge-url-format)))

(defun hpg/loaded-relations ()
  "Return `hyperdrive-peer-graph-relations' if loaded."
  (and (not (processp hpg/relations))
       (not (timerp hpg/relations))
       hpg/relations))

(cl-defun hpg/revert-buffers (&key (update-history-p t))
  "Revert peer graph buffers.
Reload data and redisplays `hyperdrive-peer-graph-mode' and
`hyperdrive-peer-graph-list-mode' buffers.  With
UPDATE-HISTORY-P, update `hyperdrive-peer-graph-history'."
  (clrhash hpg/data-cache)
  ;; TODO: How should we handle refreshing the graph/list when it's not visible?
  ;; Should we display an "outdated" warning?
  (when (get-buffer-window hpg/buffer-name 'visible)
    (hpg/draw-loading-buffer))
  (when (get-buffer-window hpg/list-buffer-name 'visible)
    (hpg/list-draw-loading-buffer))
  (hpg/load :finally (lambda ()
                       (when update-history-p (hpg/history-update))
                       (when (get-buffer-window hpg/buffer-name 'visible)
                         (hpg/draw-graph))
                       (when (get-buffer-window hpg/list-buffer-name 'visible)
                         (hpg/draw-list))
                       (hpg/refresh-menu))))

;;;;; Minor mode

;; The auto-resizing logic is adapted from `image-mode', but instead of scaling
;; the image to fit the new window size, it re-renders the image with graphviz
;; to preserve the aspect ratio.  Perhaps this logic belongs in
;; `hyperdrive-sbb', but the call to `hpg/draw-graph' lands it in this file.

(defcustom hpg/auto-resize-on-window-resize 1
  "Non-nil to resize the graph when the window's dimensions change.
Non-nil value may be the number of seconds to wait before resizing."
  :type '(choice (const :tag "No auto-resize on window size change" nil)
                 (number :tag "Wait for number of seconds before resize" 1)))

(defvar hpg/auto-resize-timer nil
  "Timer for `hyperdrive-peer-graph-auto-resize-on-window-resize' option.")

(defun hpg//window-state-change (window)
  "Copy of `image--window-state-change'."
  (when (numberp hpg/auto-resize-on-window-resize)
    (when hpg/auto-resize-timer
      (cancel-timer hpg/auto-resize-timer))
    (setq hpg/auto-resize-timer
          (run-with-idle-timer hpg/auto-resize-on-window-resize nil
                               #'hpg/fit-to-window window))))

(defvar hpg/fit-to-window-lock nil
  "Lock for `hyperdrive-peer-graph-fit-to-window' timer function.")

(defun hpg/fit-to-window (window)
  "Adapted from `image-fit-to-window', accepting WINDOW."
  (when (and (window-live-p window)
             ;; Don't resize anything if we're in the minibuffer
             ;; (which may transitively change the window sizes if you
             ;; hit TAB, for instance).
             (not (minibuffer-window-active-p (selected-window)))
             ;; Don't resize if there's a message in the echo area.
             (not (current-message)))
    (with-current-buffer (window-buffer window)
      (when (derived-mode-p 'hpg/mode)
        (let ((spec
               ;; Copied from `image-get-display-property'.
               (get-char-property (point-min) 'display
                                  ;; There might be different images for different displays.
                                  (if (eq (window-buffer) (current-buffer))
                                      (selected-window)))))
          (when (eq (car-safe spec) 'image)
            (let* ((image-width  (plist-get (cdr spec) :max-width))
                   (image-height (plist-get (cdr spec) :max-height))
                   (edges (window-inside-pixel-edges window))
                   (window-width  (- (nth 2 edges) (nth 0 edges)))
                   (window-height (- (nth 3 edges) (nth 1 edges))))
              ;; If the size has been changed manually (with `+'/`-'),
              ;; then :max-width/:max-height is nil.  In that case, do
              ;; no automatic resizing.
              (when (and (or (/= image-width window-width)
                             (/= image-height window-height))
                         ;; Don't do resizing if we have a manual
                         ;; rotation (from the `r' command), either.
                         (not (plist-get (cdr spec) :rotation)))
                (unless hpg/fit-to-window-lock
                  (unwind-protect
                      (progn
                        (setq-local hpg/fit-to-window-lock t)
                        ;; Re-render graph with graphviz.
                        (hyperdrive-peer-graph-draw-graph))
                    (setq hpg/fit-to-window-lock nil)))))))))))

(defun hpg/revert-buffer-function (&optional _ignore-auto _noconfirm)
  "Revert peer graph buffers."
  (hpg/revert-buffers))

(defvar-keymap hpg/parent-mode-map
  :doc "`hyperdrive-peer-graph-mode', `hyperdrive-peer-graph-list-mode' keys."
  ;; It's easy to accidentally trigger drag events when clicking.
  "<drag-mouse-1>" #'hpg/view-follow-link
  "<mouse-1>" #'hpg/view-follow-link
  "?"  #'hpg/menu
  "R" #'hpg/set-root-hyperdrive
  "L" #'hpg/list
  "G" #'h/peer-graph
  "l" #'hpg/history-back
  "r" #'hpg/history-forward
  "h s" #'hpg/set-sources-max-hops
  "h b" #'hpg/set-blockers-max-hops
  "o a" #'hpg/paths-only-to-add
  "o r" #'hpg/paths-only-to-remove
  "s s" #'hpg/set-show-sources-p
  "s b" #'hpg/set-show-blockers-p
  "s x" #'hpg/set-show-blocked-p
  "S" #'hpg/set-shortest-paths-p)

(defvar hpg/mode-map (make-composed-keymap hpg/parent-mode-map special-mode-map)
  "Local keymap for `hyperdrive-peer-graph-mode' buffers.")

(define-derived-mode hpg/mode h/sbb-view-mode
  '("Hyperdrive-peer-graph")
  "Major mode for viewing Hyperdrive peer graph."
  :group 'hyperdrive
  :interactive nil
  (setq-local revert-buffer-function #'hpg/revert-buffer-function)
  ;; Resize image to fit window when window is resized.
  (when hpg/auto-resize-on-window-resize
    (add-hook 'window-state-change-functions #'hpg//window-state-change nil t)))

;;;;; Graph commands

(defun hpg/view-follow-link (event)
  "Follow link at EVENT's position."
  (interactive "e")
  (when-let ((hyperdrive (h/at-point event)))
    (setf hpg/root-hyperdrive hyperdrive)
    (hpg/revert-buffers)))

;;;; Peer List

;;;;; Columns

;; These forms define the columns used to display items with `taxy-magit-section'.

(eval-and-compile
  (taxy-magit-section-define-column-definer "hyperdrive-peer-graph"))

(hpg/define-column "Peer" ()
  (h//format-preferred
   (h/url-hyperdrive
    (h/sbb-relation-to
     item))))

(hpg/define-column "Source" ()
  (let* ((directp (h/sbb-relation-direct-p
                   (h/public-key hpg/root-hyperdrive)
                   'sources
                   ;; Item is filtered to one type: get original relation.
                   (gethash (h/sbb-relation-to item) hpg/relations)))
         (text (if directp
                   (propertize "[Source]" 'face 'h/sbb-source)
                 "[      ]")))
    (if (h/writablep hpg/root-hyperdrive)
        (let ((from hpg/root-hyperdrive)
              (to (h/url-hyperdrive (h/sbb-relation-to item))))
          (buttonize text
                     (lambda (_)
                       (hpg/set-relation :from from :to to
                                         :type 'source :bool (not directp)))
                     nil (format "%s `%s' as a source from `%s'"
                                 (if directp "Unmark" "Mark")
                                 (h//format-preferred to)
                                 (h//format-preferred from))))
      text)))

(hpg/define-column "Blocker" ()
  (let* ((directp (h/sbb-relation-direct-p
                   (h/public-key hpg/root-hyperdrive)
                   'blockers
                   ;; Item is filtered to one type: get original relation.
                   (gethash (h/sbb-relation-to item) hpg/relations)))
         (text (if directp
                   (propertize "[Blocker]" 'face 'h/sbb-blocker)
                 "[       ]")))
    (if (h/writablep hpg/root-hyperdrive)
        (let ((from hpg/root-hyperdrive)
              (to (h/url-hyperdrive (h/sbb-relation-to item))))
          (buttonize text
                     (lambda (_)
                       (hpg/set-relation :from from :to to
                                         :type 'blocker :bool (not directp)))
                     nil (format "%s `%s' as a blocker from `%s'"
                                 (if directp "Unmark" "Mark")
                                 (h//format-preferred to)
                                 (h//format-preferred from))))
      text)))

(hpg/define-column "Blocked" ()
  (let* ((directp (h/sbb-relation-direct-p
                   (h/public-key hpg/root-hyperdrive)
                   'blocked
                   ;; Item is filtered to one type: get original relation.
                   (gethash (h/sbb-relation-to item) hpg/relations)))
         (text (if directp
                   (propertize "[Blocked]" 'face 'h/sbb-blocker)
                 "[      ]")))
    (if (h/writablep hpg/root-hyperdrive)
        (let ((from hpg/root-hyperdrive)
              (to (h/url-hyperdrive (h/sbb-relation-to item))))
          (buttonize text
                     (lambda (_)
                       (hpg/set-relation :from from :to to
                                         :type 'blocked :bool (not directp)))
                     nil (format "%s `%s' as blocked from `%s'"
                                 (if directp "Unmark" "Mark")
                                 (h//format-preferred to)
                                 (h//format-preferred from))))
      text)))

(unless hpg/columns
  (setq-default hpg/columns (get 'hpg/columns 'standard-value)))

;;;;; Functions

(defun hpg/format-hops (hops)
  "Return formatted string for HOPS, which is an integer."
  (if (= 1 hops) "1 hop" (format "%d hops" hops)))

(defun hpg/source-p (relation)
  "Return non-nil if RELATION is a source.
Return non-nil if RELATION has source paths and either has no
blocked paths or has a one-hop source path."
  (pcase-let (((cl-struct h/sbb-relation source-paths blocked-paths) relation))
    (or (and source-paths (not blocked-paths))
        (cl-loop for path in source-paths
                 ;; TODO: Make this customizable (include N-hop relations)?
                 ;; TODO: Move this logic into `sbb-filter-to-types'.
                 thereis (= 1 (length (h/sbb-path-hops path)))))))

(defun hpg/list (hyperdrive sources-max-hops blockers-max-hops)
  "Show menu for HYPERDRIVE peer graph."
  (interactive (hpg/interactive-args))
  (cond ((hpg/need-refresh-p hyperdrive sources-max-hops blockers-max-hops)
         (setf hpg/root-hyperdrive hyperdrive)
         (setf hpg/sources-max-hops sources-max-hops)
         (setf hpg/blockers-max-hops blockers-max-hops)
         (hpg/revert-buffers))
        ((not (buffer-live-p (get-buffer hpg/list-buffer-name)))
         (hpg/draw-list)))
  (pop-to-buffer hpg/list-buffer-name hpg/list-display-buffer-action))

(defun hpg/list-draw-loading-buffer ()
  "Draw loading buffer for hyperdrive peer graph."
  (with-current-buffer (hpg/list-get-buffer-create)
    (with-silent-modifications
      (erase-buffer)
      ;; TODO: Show empty list template when loading data.
      (insert "Loading hyperdrive peer graph data..."))))

(defun hpg/list-get-buffer-create ()
  "Return hyperdrive peer graph list buffer with mode enabled."
  (with-current-buffer (get-buffer-create hpg/list-buffer-name)
    (hpg/list-mode)
    (current-buffer)))

(defun hpg/draw-list ()
  "Draw potentially empty hyperdrive peer list."
  ;; TODO: Restore point.
  (with-current-buffer (hpg/list-get-buffer-create)
    (with-silent-modifications
      (save-excursion
        (erase-buffer)
        (widget-create
         'push-button
         :help-echo "Set root hyperdrive"
         :notify (lambda (&rest _ignore)
                   (setf hpg/root-hyperdrive
                         (hpg/context-root-hyperdrive :force-prompt t))
                   (hpg/revert-buffers))
         "Set root hyperdrive")
        (insert (format ":\n%s\n" (h//format-hyperdrive hpg/root-hyperdrive)))
        (apply #'widget-create 'menu-choice
               :tag "[Set sources max hops]"
               :value hpg/sources-max-hops
               :help-echo "Set sources max hops"
               :notify (lambda (widget &rest _ignore)
                         (setf hpg/sources-max-hops (widget-value widget))
                         (hpg/revert-buffers))
               ;; Invalid likely just means >6: format it like valid input.
               :void '(item :format "%t\n")
               (mapcar (lambda (n)
                         `(item :tag ,(number-to-string n) :value ,n))
                       '(0 1 2 3 4 5 6)))
        (apply #'widget-create 'menu-choice
               :tag "[Set blockers max hops]"
               :value hpg/blockers-max-hops
               :help-echo "Set blockers max hops"
               ;; Invalid likely just means >6: format it like valid input.
               :void '(item :format "%t\n")
               :notify (lambda (widget &rest _ignore)
                         (setf hpg/blockers-max-hops (widget-value widget))
                         (hpg/revert-buffers))
               (mapcar (lambda (n)
                         `(item :tag ,(number-to-string n) :value ,n))
                       '(0 1 2 3 4 5 6)))
        (put-text-property (point-min) (point-max) 'keymap widget-keymap)
        (insert "\n")
        (let ((relations (if hpg/list-apply-filters
                             (hpg/filter hpg/relations)
                           hpg/relations)))
          (if (hash-table-empty-p relations)
              (hpg/list-draw-empty-relations)
            (hpg/list-draw-taxy)))))))

(defun hpg/list-draw-taxy ()
  "Insert hyperdrive peer list at point."
  (let (format-table column-sizes)
    (cl-labels ((format-item (item)
                  (gethash item format-table))
                (make-fn (type &rest args)
                  (apply #'make-taxy-magit-section
                         :make (lambda (&rest args)
                                 (apply #'make-fn type args))
                         :format-fn #'format-item
                         :heading-face-fn
                         (lambda (_)
                           (pcase type
                             ('sources 'h/sbb-source)
                             ('blockers 'h/sbb-blocker)
                             ('blocked 'h/sbb-blocked)))
                         :level-indent 2
                         :item-indent 0
                         args))
                (shortest-sources-hops-length (relation)
                  (h/sbb-shortest-hops-length 'sources relation))
                (shortest-blockers-hops-length (relation)
                  (h/sbb-shortest-hops-length 'blockers relation))
                (shortest-blocked-hops-length (relation)
                  (h/sbb-shortest-blocked-hops-length
                   relation hpg/relations (h/public-key hpg/root-hyperdrive))))
      (let* ((relations (if hpg/list-apply-filters
                            (hpg/filter hpg/relations)
                          hpg/relations))
             (sources (h/sbb-filter-to-types relations :sourcesp t))
             (blockers (h/sbb-filter-to-types relations :blockersp t))
             (blocked-sources
              (h/sbb-filter-to-types relations :blocked-sources-p t))
             (blocked-non-sources
              (h/sbb-filter-to-types relations :blocked-non-sources-p t))
             (sources-taxy
              (thread-last
                (make-fn
                 'sources
                 :name "Sources"
                 :take (lambda (peer taxy)
                         (taxy-take-keyed
                           (list #'shortest-sources-hops-length)
                           peer taxy :key-name-fn #'hpg/format-hops)))
                taxy-emptied
                (taxy-fill (hash-table-values sources))
                (taxy-sort-taxys #'string< #'taxy-name)
                (taxy-sort #'string< #'h/sbb-relation-to)))
             (blockers-taxy
              (thread-last
                (make-fn
                 'blockers
                 :name "Blockers"
                 :take (lambda (peer taxy)
                         (taxy-take-keyed
                           (list #'shortest-blockers-hops-length)
                           peer taxy :key-name-fn #'hpg/format-hops)))
                taxy-emptied
                (taxy-fill (hash-table-values blockers))
                (taxy-sort-taxys #'string< #'taxy-name)
                (taxy-sort #'string< #'h/sbb-relation-to)))
             (blocked-sources-taxy
              (thread-last
                (make-fn
                 'blocked
                 :name "Blocked sources"
                 :take (lambda (peer taxy)
                         (taxy-take-keyed
                           (list #'shortest-blocked-hops-length)
                           peer taxy :key-name-fn #'hpg/format-hops)))
                taxy-emptied
                (taxy-fill (hash-table-values blocked-sources))
                (taxy-sort-taxys #'string< #'taxy-name)
                (taxy-sort #'string< #'h/sbb-relation-to)))
             (blocked-non-sources-taxy
              (thread-last
                (make-fn
                 'blocked
                 :name "Blocked non-sources"
                 :take (lambda (peer taxy)
                         (taxy-take-keyed
                           (list #'shortest-blocked-hops-length)
                           peer taxy :key-name-fn #'hpg/format-hops)))
                taxy-emptied
                (taxy-fill (hash-table-values blocked-non-sources))
                (taxy-sort-taxys #'string< #'taxy-name)
                (taxy-sort #'string< #'h/sbb-relation-to)))
             (blocked-taxy
              (make-fn
               'blocked
               :name "Blocked"
               :taxys (list blocked-sources-taxy blocked-non-sources-taxy)))
             (taxy (make-taxy-magit-section
                    :taxys (list sources-taxy blockers-taxy blocked-taxy)))
             (format-cons (taxy-magit-section-format-items
                           hpg/columns hpg/column-formatters taxy))
             (taxy-magit-section-insert-indent-items nil))
        (setf format-table (car format-cons))
        (setf column-sizes (cdr format-cons))
        (setf header-line-format (taxy-magit-section-format-header
                                  column-sizes hpg/column-formatters))
        (save-excursion
          (taxy-magit-section-insert taxy :initial-depth -1))
        ;; HACK: Delete first line of top-level taxy.  A correct solution would
        ;; be to not render the top-level taxy.  This hack means that
        ;; `magit-section-show-level-1' and `magit-section-show-level-1-all'
        ;; result in the buffer appearing empty and the other commands
        ;; `magit-section-show-level-*' show one fewer level than expected.
        (delete-line)))))

(defun hpg/list-draw-empty-relations ()
  "Insert suggestion to include more peers at point."
  (setf header-line-format nil)
  (insert "No peers found.\n")
  (when (equal 0 hpg/sources-max-hops)
    (insert "\n  - Click above to increase max hops for sources."))
  (when (equal 0 hpg/blockers-max-hops)
    (insert "\n  - Click above to increase max hops for blockers."))
  (unless (and (equal hpg/show-sources-p hpg/show-sources-p-default)
               (equal hpg/show-blockers-p hpg/show-blockers-p-default)
               (equal hpg/show-blocked-p hpg/show-blocked-p-default)
               (equal hpg/shortest-paths-p hpg/shortest-paths-p-default)
               (equal hpg/paths-only-to nil))
    (insert (format
             "\n  - %s"
             (propertize
              (buttonize
               "[Reset filters]"
               (lambda (_)
                 (setf hpg/show-sources-p hpg/show-sources-p-default
                       hpg/show-blockers-p hpg/show-blockers-p-default
                       hpg/show-blocked-p hpg/show-blocked-p-default
                       hpg/shortest-paths-p hpg/shortest-paths-p-default
                       hpg/paths-only-to nil)
                 (hpg/draw-graph)
                 (hpg/draw-list))
               nil "Reset peer graph filters")
              'face 'widget-button))))
  (when (h/writablep hpg/root-hyperdrive)
    (insert
     (format
      "\n  - Mark a peer as %s or %s or %s."
      (propertize
       (buttonize
        "[source]"
        (lambda (_)
          (apply #'hpg/set-relation
                 (hpg/set-relation-interactive-args
                  :from hpg/root-hyperdrive :type 'source :bool t)))
        nil "Mark a peer as a source")
       'face 'widget-button)
      (propertize
       (buttonize
        "[blocker]"
        (lambda (_)
          (apply #'hpg/set-relation
                 (hpg/set-relation-interactive-args
                  :from hpg/root-hyperdrive :type 'blocker :bool t)))
        nil "Mark a peer as a blocker")
       'face 'widget-button)
      (propertize
       (buttonize
        "[blocked]"
        (lambda (_)
          (apply #'hpg/set-relation
                 (hpg/set-relation-interactive-args
                  :from hpg/root-hyperdrive :type 'blocked :bool t)))
        nil "Mark a peer as blocked")
       'face 'widget-button)))))

;;;;; Minor mode

(defun hpg/list-revert-buffer-function (&optional _ignore-auto _noconfirm)
  "Revert peer graph list buffers."
  (hpg/revert-buffers))

(defvar hpg/list-mode-map
  (make-composed-keymap hpg/parent-mode-map magit-section-mode-map)
  "Local keymap for `hyperdrive-peer-graph-list-mode' buffers.")

(define-derived-mode hpg/list-mode magit-section-mode
  '("Hyperdrive-peer-graph")
  "Major mode for viewing Hyperdrive peer graph."
  :group 'hyperdrive
  :interactive nil
  (setq-local revert-buffer-function #'hpg/list-revert-buffer-function))

;;;; History

(defvar hpg/history '() "List of peer graph history elements.")
(defvar hpg/history-position 0 "Index of current place in history.")

(defun hpg/history-update ()
  "Update `hyperdrive-peer-graph-history' list.
Push an alist history item with the current values of

- `hyperdrive-peer-graph-root-hyperdrive'
- `hyperdrive-peer-graph-sources-max-hops'
- `hyperdrive-peer-graph-blockers-max-hops'
- `hyperdrive-peer-graph-show-sources-p'
- `hyperdrive-peer-graph-show-blockers-p'
- `hyperdrive-peer-graph-show-blocked-p'
- `hyperdrive-peer-graph-shortest-paths-p'
- `hyperdrive-peer-graph-paths-only-to'

If `hyperdrive-peer-graph-root-hyperdrive' is
`hyperdrive-equal-p' to the most recently pushed element, replace
the latest element instead of pushing a new element."
  (let* ((elt `((hpg/root-hyperdrive . ,hpg/root-hyperdrive)
                (hpg/sources-max-hops . ,hpg/sources-max-hops)
                (hpg/blockers-max-hops . ,hpg/blockers-max-hops)
                (hpg/show-sources-p . ,hpg/show-sources-p)
                (hpg/show-blockers-p . ,hpg/show-blockers-p)
                (hpg/show-blocked-p . ,hpg/show-blocked-p)
                (hpg/shortest-paths-p . ,hpg/shortest-paths-p)
                (hpg/paths-only-to . ,hpg/paths-only-to)))
         (current-history-item (nth hpg/history-position hpg/history))
         (current-root (map-elt current-history-item 'hpg/root-hyperdrive)))
    (if (and current-root (h/equal-p hpg/root-hyperdrive current-root))
        (setf (nth hpg/history-position hpg/history) elt)
      (setf hpg/history (cons elt (nthcdr hpg/history-position hpg/history)))
      (setf hpg/history-position 0))))

(defun hpg/history-back (&optional arg)
  "Jump to previous item in `hyperdrive-peer-graph-history'.
With numeric ARG, or interactively with universal prefix argument
\\[universal-argument], jump ARG number of times."
  (interactive "p")
  (unless (car hpg/history) (h/user-error "Peer graph history empty"))
  (let* ((len (length hpg/history))
         (new-position (min (1- len) (+ hpg/history-position arg))))
    (when (= new-position (1- len))
      (h/message "At beginning of peer graph history"))
    (mapc (pcase-lambda (`(,sym . ,val)) (set sym val))
          (nth new-position hpg/history))
    (setf hpg/history-position new-position)
    (hpg/revert-buffers :update-history-p nil)))

(defun hpg/history-forward (&optional arg)
  "Jump to next item in `hyperdrive-peer-graph-history'.
With numeric ARG, or interactively with universal prefix argument
\\[universal-argument], jump ARG number of times."
  (interactive "p")
  (unless (car hpg/history) (h/user-error "Peer graph history empty"))
  (let ((new-position (max 0 (- hpg/history-position arg))))
    (when (= new-position 0)
      (h/message "At end of peer graph history"))
    (mapc (pcase-lambda (`(,sym . ,val)) (set sym val))
          (nth new-position hpg/history))
    (setf hpg/history-position new-position)
    (hpg/revert-buffers :update-history-p nil)))

;;;; Transient UI

(defcustom h/peer-graph-menu-display-action '(display-buffer-in-side-window
                                              (side . right)
                                              (dedicated . t)
                                              (inhibit-same-window . t))
  "Display action for hyperdrive peer graph transient menu."
  :type display-buffer--action-custom-type
  :set (lambda (option value)
         (set-default option value)
         (when-let ((prefix (get 'hpg/menu 'transient--prefix)))
           (eieio-oset prefix 'display-action hpg/menu-display-action))))

(transient-define-prefix hyperdrive-peer-graph-menu
  (hyperdrive sources-max-hops blockers-max-hops)
  "Show menu for HYPERDRIVE peer graph."
  :info-manual "(hyperdrive) Explore peer graph"
  :display-action h/peer-graph-menu-display-action
  :mode-line-format nil
  ;; :transient-non-suffix t
  :refresh-suffixes t
  ["Hyperdrive peer graph"
   :pad-keys t
   ;; TODO: When changing `hpg/root-hyperdrive', reset local variables to default values?
   ("R" hpg/set-root-hyperdrive)
   ("L" "List" hpg/menu-display-list)
   ("G" "Graph" hpg/menu-display-graph)
   ("g" "Reload" hpg/reload)
   ("l" hpg/menu-history-back)
   ("r" hpg/menu-history-forward)]
  ["Max hops"
   ("h s" hpg/set-sources-max-hops)
   ("h b" hpg/set-blockers-max-hops)]
  ["Paths only to"
   (:info #'hpg/format-paths-only-to :format "%d")
   ("o a" "Add" hpg/paths-only-to-add)
   ("o r" "Remove" hpg/paths-only-to-remove)]
  ["Show type"
   ("s s" hpg/set-show-sources-p)
   ("s b" hpg/set-show-blockers-p)
   ("s x" hpg/set-show-blocked-p)]
  ["Options"
   ("S" hpg/set-shortest-paths-p)]
  (interactive (hpg/interactive-args))
  (setf hpg/root-hyperdrive hyperdrive)
  (setf hpg/sources-max-hops sources-max-hops)
  (setf hpg/blockers-max-hops blockers-max-hops)
  (hpg/load :finally #'hpg/refresh-menu)
  (transient-setup 'hyperdrive-peer-graph-menu nil nil :scope hyperdrive))

(transient-define-suffix hpg/set-root-hyperdrive ()
  :transient t
  :description
  (lambda () (format "Root: %s" (h//format hpg/root-hyperdrive)))
  (interactive)
  (setf hpg/root-hyperdrive (h/read-hyperdrive :default hpg/root-hyperdrive))
  (hpg/revert-buffers))

(transient-define-suffix hpg/set-sources-max-hops ()
  :transient t
  :description
  (lambda ()
    (format (concat "Sources: "
                    (propertize "%d" 'face 'transient-argument))
            hpg/sources-max-hops))
  (interactive)
  (setf hpg/sources-max-hops (hpg/read-max-hops 'sources))
  (hpg/revert-buffers))

(transient-define-suffix hpg/set-blockers-max-hops ()
  :transient t
  :description
  (lambda ()
    (format (concat "Blockers: "
                    (propertize "%d" 'face 'transient-argument))
            hpg/blockers-max-hops))
  (interactive)
  (setf hpg/blockers-max-hops (hpg/read-max-hops 'blockers))
  (hpg/revert-buffers))

(transient-define-suffix hpg/menu-display-graph ()
  :inapt-if-not #'hpg/loaded-relations
  :transient t
  (interactive)
  (h/peer-graph hpg/root-hyperdrive hpg/sources-max-hops hpg/blockers-max-hops))

(transient-define-suffix hpg/menu-display-list ()
  :inapt-if-not #'hpg/loaded-relations
  :transient t
  (interactive)
  (hpg/list hpg/root-hyperdrive hpg/sources-max-hops hpg/blockers-max-hops))

(transient-define-suffix hpg/reload ()
  :inapt-if-not #'hpg/loaded-relations
  :transient t
  (interactive)
  (hpg/revert-buffers))

(transient-define-suffix hpg/menu-history-back ()
  :inapt-if-not #'hpg/loaded-relations
  :description "Back"
  :transient t
  (interactive)
  (call-interactively #'hpg/history-back))

(transient-define-suffix hpg/menu-history-forward ()
  :inapt-if-not #'hpg/loaded-relations
  :description "Forward"
  :transient t
  (interactive)
  (call-interactively #'hpg/history-forward))

(transient-define-suffix hpg/set-shortest-paths-p ()
  :transient t
  :description (lambda ()
                 (format "Shortest paths: %s"
                         (if hpg/shortest-paths-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/shortest-paths-p)
  (if hpg/shortest-paths-p
      (message "SHORTEST PATHS")
    (message "ALL PATHS"))
  (hpg/history-update)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/set-show-sources-p ()
  :transient t
  :description (lambda ()
                 (format "Sources: %s"
                         (if hpg/show-sources-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-sources-p)
  (if hpg/show-sources-p
      (message "SHOW SOURCES")
    (message "HIDE SOURCES"))
  (hpg/history-update)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/set-show-blockers-p ()
  :transient t
  :description (lambda ()
                 (format "Blockers: %s"
                         (if hpg/show-blockers-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-blockers-p)
  (if hpg/show-blockers-p
      (message "SHOW BLOCKERS")
    (message "HIDE BLOCKERS"))
  (hpg/history-update)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/set-show-blocked-p ()
  :transient t
  :description (lambda ()
                 (format "Blocked: %s"
                         (pcase-exhaustive hpg/show-blocked-p
                           ('sources
                            (propertize "sources" 'face 'transient-argument))
                           ('non-sources
                            (propertize "non-sources" 'face 'transient-argument))
                           ('all
                            (propertize "all" 'face 'transient-argument))
                           ('nil (propertize "no" 'face 'transient-inactive-value)))))
  (interactive)
  ;; Cycle arguments
  (pcase-exhaustive hpg/show-blocked-p
    ('sources (setf hpg/show-blocked-p 'non-sources)
              (message "BLOCKED NON-SOURCES"))
    ('non-sources (setf hpg/show-blocked-p 'all)
                  (message "ALL BLOCKED"))
    ('all (setf hpg/show-blocked-p nil)
          (message "HIDE BLOCKED"))
    ('nil (setf hpg/show-blocked-p 'sources)
          (message "BLOCKED SOURCES")))
  (hpg/history-update)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(defun hpg/format-paths-only-to ()
  (string-join
   (mapcar (lambda (hyperdrive)
             (format "     - %s%s" (h//format hyperdrive)
                     (cond ((not (hpg/loaded-relations)) "")
                           ((not (gethash (h/public-key hyperdrive)
                                          (hpg/filter hpg/relations)))
                            (propertize " (Not among filtered relations)"
                                        'face 'error))
                           (t ""))))
           hpg/paths-only-to)
   "\n"))

(transient-define-suffix hpg/paths-only-to-add (hyperdrive)
  "Add HYPERDRIVE to `hpg/paths-only-to' and reload.
Only drives not in `hpg/paths-only-to' are offered for completion."
  :transient t
  :inapt-if-not (lambda ()
                  (and (hpg/loaded-relations)
                       (/= (hash-table-count hpg/relations)
                           (length hpg/paths-only-to))))
  (interactive
   (list (h/read-hyperdrive :prompt "Paths only to (add)"
           :predicate
           (lambda (hyperdrive)
             (unless (cl-member hyperdrive hpg/paths-only-to :test #'h/equal-p)
               (catch 'break
                 (maphash (lambda (id _)
                            (when (string= (h/public-key hyperdrive) id)
                              (throw 'break t)))
                          hpg/relations)))))))
  (push hyperdrive hpg/paths-only-to)
  (hpg/history-update)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/paths-only-to-remove (hyperdrive allp)
  "Remove HYPERDRIVE from `hpg/paths-only-to' and reload."
  :transient t
  :inapt-if-not (lambda ()
                  (and hpg/paths-only-to (hpg/loaded-relations)))
  (interactive (list (or current-prefix-arg
                         ;; HACK: Skip prompt if `current-prefix-arg'.
                         (if (length= hpg/paths-only-to 1)
                             (car hpg/paths-only-to)
                           (h/read-hyperdrive :prompt "Paths only to (remove)"
                             :predicate
                             (lambda (hyperdrive)
                               (cl-member hyperdrive hpg/paths-only-to
                                          :test #'h/equal-p)))))
                     current-prefix-arg))
  (setf hpg/paths-only-to
        (and (not allp)
             (cl-delete hyperdrive hpg/paths-only-to :test #'h/equal-p)))
  (hpg/history-update)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(defun hpg/refresh-menu ()
  "Refresh `hyperdrive-peer-graph-menu' if it's open."
  (when (transient-active-prefix 'hyperdrive-peer-graph-menu)
    (transient--refresh-transient)))

;;; Footer:

(provide 'hyperdrive-peer-graph)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hpg/"  . "hyperdrive-peer-graph-")
;;   ("hpg//"  . "hyperdrive-peer-graph--")
;;   ("h/"   . "hyperdrive-"))
;; End:

;;; hyperdrive-peer-graph.el ends here
