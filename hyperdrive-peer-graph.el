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

(require 'hyperdrive-fons-view)
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

(defcustom hpg/show-blocked-p-default t
  "Default setting to show blocked."
  :type 'boolean)

(defcustom hpg/show-all-blocked-p-default nil
  "Default setting to show all blocked, not just sources."
  :type 'boolean)

(defcustom hpg/shortest-path-p-default t
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

(defconst hpg/data-filename "/.well-known/peer-graph.json"
  "Hyperdrive filename to search for peer graph data.")

(defvar hpg/root-hyperdrive nil)
(defvar hpg/relations nil)

(defvar hpg/sources-max-hops hpg/sources-max-hops-default)
(defvar hpg/blockers-max-hops hpg/blockers-max-hops-default)

(defvar hpg/show-sources-p hpg/show-sources-p-default)
(defvar hpg/show-blockers-p hpg/show-blockers-p-default)
(defvar hpg/show-blocked-p hpg/show-blocked-p-default)
(defvar hpg/show-all-blocked-p hpg/show-all-blocked-p-default)

(defvar hpg/shortest-path-p hpg/shortest-path-p-default)
(defvar hpg/paths-only-to nil)

;;;; Functions:

(defvar hpg/data-cache (make-hash-table :test 'equal)
  "Hash table mapping public keys to JSON hops.")

(cl-defun hpg/data (hyperdrive &key then)
  "Return peer graph data for HYPERDRIVE then call THEN.
THEN will be called with the parsed JSON object as its sole
argument.  If error, demote it and call THEN with nil argument.
If THEN is nil or \\+`sync', the request will be synchronous, and
errors will be demoted.  If data for HYPERDRIVE is already in
`hyperdrive-peer-graph-data-cache', use it and send no request."
  (declare (indent defun))
  ;; TODO: Add else handler so that we can replace the loading screen.
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
             (entries (h//bee-exec hyperdrive commands :then then))
           (error
            ;; Despite error, call THEN so `pending-relations' gets decremented.
            (let ((inhibit-message t))
              (h/message "Error getting peer graph data for hyperdrive %s: %S"
                         (h/url hyperdrive) err)
              (funcall then nil)))))

        (_
         (h//bee-exec hyperdrive commands
           :then (lambda (json) (funcall then (entries json)))
           :else (lambda (plz-error)
                   (h/message "Error getting peer graph data for %s: %S"
                              (h/url hyperdrive) plz-error)
                   (funcall then nil))))))))

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
  (cl-labels ((default-hyperdrive ()
                (or hpg/root-hyperdrive
                    (and h/current-entry
                         (he/hyperdrive h/current-entry)))))
    (let* ((from (or from
                     (h/read-hyperdrive
                       :predicate
                       (if to
                           (lambda (hyperdrive)
                             (and (h/writablep hyperdrive)
                                  (not (h/equal-p to hyperdrive))))
                         #'h/writablep)
                       :default (default-hyperdrive)
                       :prompt "From hyperdrive")))
           (to (or to
                   (h/read-hyperdrive
                     :predicate (lambda (hyperdrive)
                                  (not (h/equal-p hyperdrive from)))
                     :default (default-hyperdrive)
                     :prompt "To hyperdrive")))
           (type (or type (hpg/read-relation-type)))
           (bool (or bool (not current-prefix-arg))))
      (list :from from :to to :type type :bool bool))))

(defun hpg/sources-hops-fn (from then)
  "Asynchronously get source hops from FROM.
Call THEN with a list of TOs."
  (hpg/data (h/create :public-key from)
    :then (lambda (data) (funcall then (map-elt data 'sources)))))

(defun hpg/blockers-hops-fn (from then)
  "Asynchronously get blocker hops from FROM.
Call THEN with a list of TOs."
  (hpg/data (h/create :public-key from)
    :then (lambda (data) (funcall then (map-elt data 'blockers)))))

(cl-defun hpg/blocked-hops-fn (blocker then)
  "Asynchronously get blocks from BLOCKER.
Call THEN with a list of block IDs."
  (hpg/data (h/create :public-key blocker)
    :then (lambda (data) (funcall then (map-elt data 'blocked)))))

(cl-defun hpg/insert-relation (public-key relations root)
  "Insert display string for PUBLIC-KEY in current buffer.
RELATION may be a hash table of `fons-relation' structs mapped by
\\+`public-key'.  ROOT is the \\+`public-key' of the root node."
  (pcase-let*
      ((hyperdrive (h/url-hyperdrive public-key))
       (rootp (equal public-key root))
       ((or (guard rootp)
            (cl-struct
             fons-relation blocker-paths blocked-paths source-paths))
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
         label `(tr nil (td nil (font ((color . ,h/fons-view-sources-color))
                                      "source")))))
      (when blocker-paths
        (dom-append-child
         label `(tr nil (td nil (font ((color . ,h/fons-view-blockers-color))
                                      "blocker")))))
      (when blocked-paths
        (dom-append-child
         label `(tr nil (td nil (font ((color . ,h/fons-view-blocked-color))
                                      "blocked"))))))
    (insert (format "%s [label=<\n  " public-key))
    (dom-print label)
    (insert (format "\n>, href=\"%s\", color=\"%s\", bgcolor=\"%s\", shape=\"none\", margin=\"0\", style=\"filled\"];\n" public-key (face-attribute 'default :background)  (face-attribute 'default :background)))))

(cl-defun hpg/relations (root &key finally sources-max-hops blockers-max-hops)
  "Load relations from ROOT and call FINALLY.
FINALLY should be a function which accepts a single argument, a
hash table of `fons-relation' structs keyed by public key.
SOURCES-MAX-HOPS and BLOCKERS-MAX-HOPS are the maximum number of
hops to traverse for sources and blockers, respectively."
  (fons-relations root
    :hops-fn #'hpg/blockers-hops-fn :type 'blockers
    :max-hops blockers-max-hops :finally
    (lambda (relations)
      (fons-blocked root relations
        :hops-fn #'hpg/blocked-hops-fn :finally
        (lambda (relations)
          (when-let* ((root-public-key (h/public-key hpg/root-hyperdrive))
                      (relation-to-root (gethash root-public-key relations))
                      (blocked-paths-to-root
                       (fons-relation-paths-of-type 'blocked relation-to-root)))
            (dolist (path blocked-paths-to-root)
              (h/message
               "Ignoring blocked path to root %s from %s"
               (h/url hpg/root-hyperdrive)
               (h/url (h/url-hyperdrive (fons-blocked-path-blocker path))))
              (remhash root-public-key relations)))
          (fons-relations root
            :relations relations :type 'sources :max-hops sources-max-hops
            :hops-fn #'hpg/sources-hops-fn :finally finally))))))

(defun hpg/filter (relations)
  "Return filtered RELATIONS."
  ;; TODO: Make filters customizable
  ;; Apply shortest path filter first.
  (when hpg/shortest-path-p
    (cl-callf fons-filter-shortest-path relations
      (h/public-key hpg/root-hyperdrive)))
  (unless (and hpg/show-sources-p hpg/show-blockers-p hpg/show-blocked-p hpg/show-all-blocked-p)
    (cl-callf fons-filter-to-types relations
      :sourcesp hpg/show-sources-p
      :blockersp hpg/show-blockers-p
      :blockedp hpg/show-blocked-p
      :all-blocked-p (and hpg/show-blocked-p hpg/show-all-blocked-p)))
  ;; Apply `hpg/paths-only-to' last
  (cl-callf2 fons-filter-paths-only-to
      (mapcar #'h/public-key hpg/paths-only-to) relations)
  relations)

;;;;; Reading user input

(defun hpg/read-relation-type ()
  "Return one of \\+`source', \\+`blocker', or \\+`blocked'."
  (let ((string (completing-read
                 (format-prompt "Relation type" "source")
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

;;;; Peer List

;;;;; Columns

;; These forms define the columns used to display items with `taxy-magit-section'.

(eval-and-compile
  (taxy-magit-section-define-column-definer "hyperdrive-peer-graph"))

(hpg/define-column "Peer" ()
  (h//format-preferred
   (h/url-hyperdrive
    (fons-relation-to
     item))))

(hpg/define-column "Source" ()
  (let* ((directp (fons-relation-direct-p
                   hpg/root-hyperdrive 'sources
                   ;; Item is filtered to one type: get original relation.
                   (gethash (fons-relation-to item) hpg/relations)))
         (text (if directp
                   (propertize "[Source]" 'face 'h/fons-source)
                 "[      ]")))
    (if (h/writablep hpg/root-hyperdrive)
        (buttonize text
                   (lambda (_)
                     (hpg/set-relation
                      :from hpg/root-hyperdrive
                      :to (h/url-hyperdrive (fons-relation-to item))
                      :type 'source :bool (not directp))))
      text)))

(hpg/define-column "Blocker" ()
  (let* ((directp (fons-relation-direct-p
                   hpg/root-hyperdrive 'blockers
                   ;; Item is filtered to one type: get original relation.
                   (gethash (fons-relation-to item) hpg/relations)))
         (text (if directp
                   (propertize "[Blocker]" 'face 'h/fons-blocker)
                 "[       ]")))
    (if (h/writablep hpg/root-hyperdrive)
        (buttonize text
                   (lambda (_)
                     (hpg/set-relation
                      :from hpg/root-hyperdrive
                      :to (h/url-hyperdrive (fons-relation-to item))
                      :type 'blocker :bool (not directp))))
      text)))

(hpg/define-column "Blocked" ()
  (let* ((directp (fons-relation-direct-p
                   hpg/root-hyperdrive 'blocked
                   ;; Item is filtered to one type: get original relation.
                   (gethash (fons-relation-to item) hpg/relations)))
         (text (if directp
                   (propertize "[Blocked]" 'face 'h/fons-blocker)
                 "[      ]")))
    (if (h/writablep hpg/root-hyperdrive)
        (buttonize text
                   (lambda (_)
                     (hpg/set-relation
                      :from hpg/root-hyperdrive
                      :to (h/url-hyperdrive (fons-relation-to item))
                      :type 'blocked :bool (not directp))))
      text)))

(unless hpg/columns
  (setq-default hpg/columns (get 'hpg/columns 'standard-value)))

;;;;; Functions

(defun fons-shortest-hops-length (type relation)
  "Return the minimum number of TYPE hops in RELATION."
  (cl-loop for path in (fons-relation-paths-of-type type relation)
           minimize (length (fons-path-hops path))))

(defun fons-shortest-sources-hops-length (relation)
  "Return the shortest number of source hops for RELATION."
  (fons-shortest-hops-length 'sources relation))

(defun fons-shortest-blockers-hops-length (relation)
  "Return the minimum number of blocker hops in RELATION."
  (fons-shortest-hops-length 'blockers relation))

(cl-defun fons-shortest-blocked-hops-length (relation)
  "Return the minimum number of blocked hops in RELATION.
A blocked hop includes the number of hops to the blocker."
  ;; TODO: Generalize this function so it doesn't rely on `hpg/...' variables.
  (when-let*
      ((blocked-paths (fons-relation-blocked-paths relation))
       (blockers
        (mapcar (lambda (path)
                  (let ((blocker-public-key (fons-blocked-path-blocker path)))
                    (when (equal (h/public-key hpg/root-hyperdrive)
                                 blocker-public-key)
                      ;; Direct block from root: return 1.
                      (cl-return-from fons-shortest-blocked-hops-length 1))
                    (gethash blocker-public-key hpg/relations)))
                blocked-paths)))
    (1+ (cl-loop
         for blocker in blockers
         minimize (fons-shortest-hops-length 'blockers blocker)))))

(defun hpg/format-hops (hops)
  "Return formatted string for HOPS, which is an integer."
  (if (= 1 hops) "1 hop" (format "%d hops" hops)))

(defun hpg/source-p (relation)
  "Return non-nil if RELATION is a source.
Return non-nil if RELATION has source paths and either has no
blocked paths or has a one-hop source path."
  (pcase-let (((cl-struct fons-relation source-paths blocked-paths) relation))
    (or (and source-paths (not blocked-paths))
        (cl-loop for path in source-paths
                 ;; TODO: Make this customizable (include N-hop relations)?
                 ;; TODO: Move this logic into `fons-filter-to-types'.
                 thereis (= 1 (length (fons-path-hops path)))))))

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
  ;; TODO: Add `hpg/list-display-buffer-action'
  (pop-to-buffer hpg/list-buffer-name hpg/display-buffer-action))

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
         :notify (lambda (&rest ignore)
                   (setf hpg/root-hyperdrive
                         (hpg/context-root-hyperdrive :force-prompt t))
                   (hpg/revert-buffers))
         "Set root hyperdrive")
        (insert (format ":\n%s\n" (h//format-hyperdrive hpg/root-hyperdrive)))
        (apply #'widget-create 'menu-choice
               :tag "[Set sources max hops]"
               :value hpg/sources-max-hops
               :help-echo "Set sources max hops"
               :notify (lambda (widget &rest ignore)
                         (setf hpg/sources-max-hops (widget-value widget))
                         (hpg/revert-buffers))
               (mapcar (lambda (n)
                         `(item :tag ,(number-to-string n) :value ,n))
                       '(0 1 2 3 4 5 6)))
        (apply #'widget-create 'menu-choice
               :tag "[Set blockers max hops]"
               :value hpg/blockers-max-hops
               :help-echo "Set blockers max hops"
               :notify (lambda (widget &rest ignore)
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
            (hpg/list-draw-taxy relations)))))))

(defun hpg/list-draw-taxy (relations)
  "Insert hyperdrive peer list for RELATIONS at point."
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
                             ('sources 'hyperdrive-fons-source)
                             ('blockers 'hyperdrive-fons-blocker)
                             ('blocked 'hyperdrive-fons-blocked)))
                         :level-indent 2
                         :item-indent 0
                         args)))
      (let* ((relations (if hpg/list-apply-filters
                            (hpg/filter hpg/relations)
                          hpg/relations))
             (sources (fons-filter-to-types relations :sourcesp t))
             (blockers (fons-filter-to-types relations :blockersp t))
             (blocked (fons-filter-to-types
                       relations :blockedp t :all-blocked-p t))
             (sources-taxy
              (thread-last
                (make-fn
                 'sources
                 :name "Sources"
                 :take (lambda (peer taxy)
                         (taxy-take-keyed
                           (list #'fons-shortest-sources-hops-length)
                           peer taxy :key-name-fn #'hpg/format-hops)))
                taxy-emptied
                (taxy-fill (hash-table-values sources))
                (taxy-sort #'string< #'fons-relation-to)))
             (blockers-taxy
              (thread-last
                (make-fn
                 'blockers
                 :name "Blockers"
                 :take (lambda (peer taxy)
                         (taxy-take-keyed
                           (list #'fons-shortest-blockers-hops-length)
                           peer taxy :key-name-fn #'hpg/format-hops)))
                taxy-emptied
                (taxy-fill (hash-table-values blockers))
                (taxy-sort #'string< #'fons-relation-to)))
             (blocked-taxy
              (thread-last
                (make-fn
                 'blocked
                 :name "Blocked"
                 :take (lambda (peer taxy)
                         (taxy-take-keyed
                           (list #'fons-shortest-blocked-hops-length)
                           peer taxy :key-name-fn #'hpg/format-hops)))
                taxy-emptied
                (taxy-fill (hash-table-values blocked))
                (taxy-sort #'string< #'fons-relation-to)))
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
               (equal hpg/show-all-blocked-p hpg/show-all-blocked-p-default)
               (equal hpg/shortest-path-p hpg/shortest-path-p-default)
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
                       hpg/show-all-blocked-p hpg/show-all-blocked-p-default
                       hpg/shortest-path-p hpg/shortest-path-p-default
                       hpg/paths-only-to nil)
                 (hpg/draw-graph)
                 (hpg/draw-list)))
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
                  :from hpg/root-hyperdrive :type 'source :bool t))))
       'face 'widget-button)
      (propertize
       (buttonize
        "[blocker]"
        (lambda (_)
          (apply #'hpg/set-relation
                 (hpg/set-relation-interactive-args
                  :from hpg/root-hyperdrive :type 'blocker :bool t))))
       'face 'widget-button)
      (propertize
       (buttonize
        "[blocked]"
        (lambda (_)
          (apply #'hpg/set-relation
                 (hpg/set-relation-interactive-args
                  :from hpg/root-hyperdrive :type 'blocked :bool t))))
       'face 'widget-button)))))

;;;;; Minor mode

(defun hpg/list-revert-buffer-function (&optional _ignore-auto _noconfirm)
  "Revert peer graph list buffers."
  (hpg/revert-buffers))

(defvar-keymap hpg/list-mode-map
  :parent hpg/mode-map
  :doc "Local keymap for `hyperdrive-peer-graph-list-mode' buffers.")

(define-derived-mode hpg/list-mode magit-section-mode
  '("Hyperdrive-peer-graph")
  "Major mode for viewing Hyperdrive peer graph."
  :group 'hyperdrive
  :interactive nil
  (setq-local revert-buffer-function #'hpg/list-revert-buffer-function))

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
    (h/fons-view (hpg/filter hpg/relations)
                 (h/public-key hpg/root-hyperdrive)
                 :insert-relation-fun #'hpg/insert-relation)))

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

(defun hpg/revert-buffer-function (&optional _ignore-auto _noconfirm)
  "Revert peer graph buffers."
  (hpg/revert-buffers))

(defvar-keymap hpg/mode-map
  :parent special-mode-map
  :doc "Local keymap for `hyperdrive-peer-graph-mode' buffers."
  ;; It's easy to accidentally trigger drag events when clicking.
  "<drag-mouse-1>" #'hpg/view-follow-link
  "<mouse-1>" #'hpg/view-follow-link
  "l" #'hpg/history-back
  "r" #'hpg/history-forward
  ;; TODO: Add bindings to set max hops, etc.
  "?" #'hpg/menu)

(define-derived-mode hpg/mode h/fons-view-mode
  '("Hyperdrive-peer-graph")
  "Major mode for viewing Hyperdrive peer graph."
  :group 'hyperdrive
  :interactive nil
  (setq-local revert-buffer-function #'hpg/revert-buffer-function))

;;;;; Graph commands

(defun hpg/view-follow-link (event)
  "Follow link at EVENT's position."
  (interactive "e")
  (when-let ((hyperdrive (h/at-point event)))
    (setf hpg/root-hyperdrive hyperdrive)
    (hpg/revert-buffers)))

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
- `hyperdrive-peer-graph-show-all-blocked-p'
- `hyperdrive-peer-graph-shortest-path-p'
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
                (hpg/show-all-blocked-p . ,hpg/show-all-blocked-p)
                (hpg/shortest-path-p . ,hpg/shortest-path-p)
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

;;;###autoload (autoload 'hyperdrive-peer-graph-menu "hyperdrive-peer-graph" nil t)
(transient-define-prefix hyperdrive-peer-graph-menu
  (hyperdrive sources-max-hops blockers-max-hops)
  "Show menu for HYPERDRIVE peer graph."
  ;; TODO: Update info manual link
  :info-manual "(hyperdrive)"
  :refresh-suffixes t
  ["Hyperdrive peer graph"
   :pad-keys t
   ;; TODO: When changing `hpg/root-hyperdrive', reset local variables to default values?
   ("R" hpg/set-root-hyperdrive)
   ("L" "Display list" hpg/menu-display-list)
   ("G" "Display graph" hpg/menu-display-graph)
   ("g" "Reload" hpg/reload)
   ("l" hpg/menu-history-back)
   ("r" hpg/menu-history-forward)]
  ["Paths only to"
   (:info #'hpg/format-paths-only-to :format "%d")
   ("o a" "Add" hpg/paths-only-to-add)
   ("o r" "Remove" hpg/paths-only-to-remove)]
  [["Sources"
    ("s s" hpg/set-show-sources-p)
    ("s m" hpg/set-sources-max-hops)]
   ["Blockers"
    ("b s" hpg/set-show-blockers-p)
    ("b m" hpg/set-blockers-max-hops)]
   ["Blocked"
    ("x s" hpg/set-show-blocked-p)
    ("x a" hpg/set-show-all-blocked-p)]]
  ["Options"
   ("S" hpg/set-shortest-path-p)]
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
    (format (concat "Max hops for sources: "
                    (propertize "%d" 'face 'transient-argument))
            hpg/sources-max-hops))
  (interactive)
  (setf hpg/sources-max-hops (hpg/read-max-hops 'sources))
  (hpg/revert-buffers))

(transient-define-suffix hpg/set-blockers-max-hops ()
  :transient t
  :description
  (lambda ()
    (format (concat "Max hops for blockers: "
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

(transient-define-suffix hpg/set-shortest-path-p ()
  :transient t
  :description (lambda ()
                 (format "Shortest paths only: %s"
                         (if hpg/shortest-path-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/shortest-path-p)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/set-show-sources-p ()
  :transient t
  :description (lambda ()
                 (format "Show sources: %s"
                         (if hpg/show-sources-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-sources-p)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/set-show-blockers-p ()
  :transient t
  :description (lambda ()
                 (format "Show blockers: %s"
                         (if hpg/show-blockers-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-blockers-p)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/set-show-blocked-p ()
  :transient t
  :description (lambda ()
                 (format "Show blocked: %s"
                         (if hpg/show-blocked-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-blocked-p)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/set-show-all-blocked-p ()
  :transient t
  :inapt-if-nil 'hpg/show-blocked-p
  :description (lambda ()
                 (format "Show all blocked: %s"
                         (if (and hpg/show-blocked-p hpg/show-all-blocked-p)
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-all-blocked-p)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(defun hpg/format-paths-only-to ()
  (string-join
   (mapcar (lambda (hyperdrive)
             (format "     - %s" (h//format hyperdrive)))
           hpg/paths-only-to)
   "\n"))

(transient-define-suffix hpg/paths-only-to-add (hyperdrive)
  "Add HYPERDRIVE to `hpg/paths-only-to' and reload.
Only drives not in `hpg/paths-only-to' are offered for completion."
  :transient t
  :inapt-if-not #'hpg/loaded-relations
  (interactive
   (list (h/read-hyperdrive :predicate
           (lambda (hyperdrive)
             (unless (cl-member hyperdrive hpg/paths-only-to :test #'h/equal-p)
               (catch 'break
                 (maphash (lambda (id _)
                            (when (string= (h/public-key hyperdrive) id)
                              (throw 'break t)))
                          hpg/relations)))))))
  (push hyperdrive hpg/paths-only-to)
  (hpg/draw-graph)
  (when hpg/list-apply-filters (hpg/draw-list)))

(transient-define-suffix hpg/paths-only-to-remove (hyperdrive allp)
  "Remove HYPERDRIVE from `hpg/paths-only-to' and reload."
  :transient t
  :inapt-if-not #'hpg/loaded-relations
  (interactive (list (or current-prefix-arg
                         ;; HACK: Skip prompt if `current-prefix-arg'.
                         (if (length= hpg/paths-only-to 1)
                             (car hpg/paths-only-to)
                           (h/read-hyperdrive :predicate
                             (lambda (hyperdrive)
                               (cl-member hyperdrive hpg/paths-only-to
                                          :test #'h/equal-p)))))
                     current-prefix-arg))
  (setf hpg/paths-only-to
        (and (not allp)
             (cl-delete hyperdrive hpg/paths-only-to :test #'h/equal-p)))
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
;;   ("hf/"  . "hyperdrive-fons-")
;;   ("h/"   . "hyperdrive-"))
;; End:

;;; hyperdrive-peer-graph.el ends here
