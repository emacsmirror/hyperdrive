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

;;;; Variables:

(defgroup hyperdrive-peer-graph nil
  "Customization of the hyperdrive peer graph."
  :group 'hyperdrive)

(defcustom hpg/default-topic "_default"
  "Special topic name used as a fallback when no topic is specified."
  :type 'string)

(defcustom hpg/sources-max-hops-default 3
  "Default number of hops to jump for sources."
  :type 'integer)

(defcustom hpg/blockers-max-hops-default 3
  "Default number of hops to jump for blockers."
  :type 'integer)

(defcustom hpg/buffer-name "*hyperdrive-peer-graph-view*"
  "Buffer name to show peer graph."
  :type 'string)

(defcustom hpg/display-buffer-action '(display-buffer-reuse-window)
  "Display buffer action for hyperdrive peer graph.
Passed to `display-buffer', which see."
  :type display-buffer--action-custom-type)

(defconst hpg/data-filename "/.well-known/peer-graph.json"
  "Hyperdrive filename to search for peer graph data.")

(defvar hpg/root-hyperdrive nil)
(defvar hpg/topic nil)
(defvar hpg/relations nil)

(defvar hpg/sources-max-hops hpg/sources-max-hops-default)
(defvar hpg/blockers-max-hops hpg/blockers-max-hops-default)

(defvar hpg/show-sources-p t)
(defvar hpg/show-blockers-p t)
(defvar hpg/show-blocked-p t)
(defvar hpg/show-all-blocked-p nil)

(defvar hpg/shortest-path-p t)
(defvar hpg/paths-only-to nil)

;;;; Functions:

(defun hpg/data (hyperdrive &key then)
  "Load peer graph data for HYPERDRIVE on TOPIC then call THEN.
THEN will be called with the parsed JSON hash table as its sole
argument.  If error, demote it and call THEN with nil argument."
  (declare (indent defun))
  ;; TODO: Add else handler so that we can replace the loading screen.
  ;; TODO: Add a queue limit.
  ;; TODO: `hpg/data' may be called multiple times for the same hyperdrive in a
  ;; single call to `fons-relations'.  If performance becomes an issue, memoize.
  (let ((entry (he//create :hyperdrive hyperdrive :path hpg/data-filename)))
    (he/api 'get entry :noquery t
      ;; Despite error, always call THEN so `pending-relations' gets decremented.
      :then (lambda (response)
              (condition-case err
                  ;; TODO: When plz adds :as 'response-with-buffer, use that.
                  (funcall then (json-parse-string
                                 (plz-response-body response)
                                 :array-type 'list))
                (json-error
                 (h/message "Error parsing peer graph data: %s\n%S"
                            (he/url entry) err)
                 (funcall then nil))))
      :else (lambda (plz-error)
              (pcase (plz-response-status (plz-error-response plz-error))
                ;; FIXME: If plz-error is a curl-error, this block will fail.
                (404
                 (h/message "No peer graph data found: %s" (he/url entry))
                 (funcall then nil))
                (_
                 ;; TODO: Put error in another buffer.  Check error 500 for malformed URLs?
                 (h/message "Error getting peer graph data: %s" (he/url entry) plz-error)
                 (funcall then nil)))))))

(defun hpg/sources-hops-fn (topic from then)
  "Asynchronously get source hops from FROM about TOPIC.
Call THEN with a list of TOs."
  (hpg/data (h/create :public-key from)
    :then (pcase-lambda ((map ("sources" sources)))
            (funcall then (map-elt sources topic)))))

(defun hpg/blockers-hops-fn (from then)
  "Asynchronously get blocker hops from FROM.
Call THEN with a list of TOs."
  (hpg/data (h/create :public-key from)
    :then (lambda (data) (funcall then (map-elt data "blockers")))))

(cl-defun hpg/blocked-hops-fn (blocker then)
  "Asynchronously get blocks from BLOCKER.
Call THEN with a list of block IDs."
  (hpg/data (h/create :public-key blocker)
    :then (lambda (data) (funcall then (map-elt data "blocked")))))

(defun hpg/label-fun (public-key)
  "Return display string for PUBLIC-KEY."
  (let ((hyperdrive (h/url-hyperdrive public-key))
        (h/formats '((petname . "%s")
                     (nickname . "%s")
                     (public-key . "%s")
                     ;; FIXME: Don't hardcode short key length.
                     (short-key . "%.8s…")
                     (seed . "%s")
                     (domains . "%s"))))
    (format "<%s<br/><FONT POINT-SIZE=\"10\">%s</FONT>>"
            (h//format-preferred hyperdrive) (h//preferred-format hyperdrive))))

(cl-defun hpg/relations (root topic &key finally sources-max-hops blockers-max-hops)
  "Load relations from ROOT about TOPIC and call FINALLY.
FINALLY should be a function which accepts a single argument, a
hash table of `fons-relation' structs keyed by public key.
SOURCES-MAX-HOPS and BLOCKERS-MAX-HOPS are the maximum number of
hops to traverse for sources and blockers, respectively."
  (fons-relations root
    :hops-fn #'hpg/blockers-hops-fn :type 'blockers
    :max-hops blockers-max-hops :finally
    (lambda (relations)
      (fons-blocked relations
        :hops-fn #'hpg/blocked-hops-fn :finally
        (lambda (relations)
          (fons-relations root
            :relations relations :type 'sources :max-hops sources-max-hops
            :hops-fn (apply-partially #'hpg/sources-hops-fn topic) :finally
            (lambda (relations)
              (funcall finally relations))))))))

(defun hpg/filter (relations)
  "Return filtered RELATIONS."
  ;; TODO: Make filters customizable
  (unless (and hpg/show-sources-p hpg/show-blockers-p hpg/show-blocked-p hpg/show-all-blocked-p)
    (cl-callf fons-filter-to-types relations
      :sourcesp hpg/show-sources-p
      :blockersp hpg/show-blockers-p
      :blockedp hpg/show-blocked-p
      :all-blocked-p (and hpg/show-blocked-p hpg/show-all-blocked-p)))
  (when hpg/shortest-path-p
    (cl-callf fons-filter-shortest-path relations))
  ;; Apply `hpg/paths-only-to' last
  (cl-callf2 fons-filter-paths-only-to
      (mapcar #'h/public-key hpg/paths-only-to) relations)
  relations)

;;;;; Reading user input

(defvar hpg/topic-history nil
  "Minibuffer history of `hyperdrive-peer-graph-read-topic'.")

(defun hpg/read-topic ()
  "Read topic string or nil with blank string."
  (let ((topic (read-string "Topic (leave blank for default topic): "
                            nil hpg/topic-history)))
    (and (not (string-blank-p topic))
         topic)))

(cl-defun hpg/context-topic (&key force-prompt)
  "Return `hyperdrive-peer-graph-topic'.
With FORCE-PROMPT, or interactively with universal prefix
argument \\[universal-argument], always prompt.
Blank string defaults to `hyperdrive-peer-graph-default-topic'."
  (if force-prompt (hpg/read-topic) hpg/topic))

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
    (read-number (format "Max hops for %s" type) default
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
         (topic (hpg/context-topic :force-prompt current-prefix-arg))
         (topic-or-root-changed
          (not (and (equal topic hpg/topic)
                    (eq root-hyperdrive hpg/root-hyperdrive))))
         (sources-max-hops (hpg/context-max-hops
                            'sources :force-prompt (or current-prefix-arg
                                                       topic-or-root-changed)))
         (blockers-max-hops (hpg/context-max-hops
                             'blockers
                             :force-prompt (or current-prefix-arg
                                               topic-or-root-changed))))
    (list topic root-hyperdrive sources-max-hops blockers-max-hops)))

;;;; Peer Graph

(defun hyperdrive-peer-graph
    (topic hyperdrive sources-max-hops blockers-max-hops)
  "Show menu for HYPERDRIVE peer graph."
  (interactive (hpg/interactive-args))
  (if (and (equal topic hpg/topic)
           hpg/root-hyperdrive
           (h/equal-p hyperdrive hpg/root-hyperdrive)
           (hpg/loaded-relations))
      (hpg/display-graph)
    (setf hpg/topic topic)
    (setf hpg/root-hyperdrive hyperdrive)
    (setf hpg/sources-max-hops sources-max-hops)
    (setf hpg/blockers-max-hops blockers-max-hops)
    (hpg/load)))

(defun hpg/load ()
  "Load `hpg/relations' and redisplay graph."
  ;; TODO: Refill name and color metadata.
  ;; TODO: If called in rapid succession, stop the requests from the first call.
  (setf hpg/relations
        (hpg/relations
         (h/public-key hpg/root-hyperdrive)
         (or hpg/topic hpg/default-topic)
         :sources-max-hops hpg/sources-max-hops
         :blockers-max-hops hpg/blockers-max-hops
         :finally
         (lambda (relations)
           (setf hpg/relations relations)
           (h/fill-metadata-all
            (cons hpg/root-hyperdrive
                  (mapcar #'h/url-hyperdrive (hash-table-keys hpg/relations)))
            :finally (lambda ()
                       (hpg/display-graph)
                       (hpg/refresh-menu))))))
  (hpg/display-loading-buffer))

(defun hpg/display-loading-buffer ()
  "Open loading buffer for hyperdrive peer graph."
  (with-current-buffer (get-buffer-create hpg/buffer-name)
    (with-silent-modifications
      (erase-buffer)
      (insert "Loading hyperdrive peer graph data...")
      (display-buffer (current-buffer) hpg/display-buffer-action))))

(defun hpg/display-graph ()
  "Open buffer displaying hyperdrive peer graph."
  (with-current-buffer (get-buffer-create hpg/buffer-name)
    (h/fons-view (hpg/filter hpg/relations)
                 (h/public-key hpg/root-hyperdrive)
                 :focus-ids (mapcar #'h/public-key hpg/paths-only-to)
                 :label-fun #'hpg/label-fun)
    (hpg/mode)
    (pop-to-buffer (current-buffer) hpg/display-buffer-action)))

(defun hpg/loaded-relations ()
  "Return `hyperdrive-peer-graph-relations' if loaded."
  (and (not (processp hpg/relations))
       hpg/relations))

;;;; Minor mode

(defun hpg/revert-buffer (&optional _ignore-auto _noconfirm)
  "Revert `hyperdrive-describe-mode' buffer.
Reload data and redisplay graph."
  (hpg/load))

(defvar-keymap hpg/mode-map
  :parent special-mode-map
  :doc "Local keymap for `hyperdrive-peer-graph-mode' buffers."
  ;; It's easy to accidentally trigger drag events when clicking.
  "<drag-mouse-1>" #'hpg/view-follow-link
  "<mouse-1>" #'hpg/view-follow-link
  "<drag-mouse-3>" #'hpg/menu-bar
  "<mouse-3>" #'hpg/menu-bar
  "?" #'hpg/menu)

(define-derived-mode hpg/mode h/fons-view-mode
  '("Hyperdrive-peer-graph")
  "Major mode for viewing Hyperdrive peer graph."
  :group 'hyperdrive
  :interactive nil
  (setq-local revert-buffer-function #'hpg/revert-buffer))

;;;;; Graph commands

(defun hpg/view-follow-link (event)
  "Follow link at EVENT's position."
  (interactive "e")
  (pcase (cadadr event)  ;; Image id from image map
    ((and (rx (group (= 52 alphanumeric))) public-key)
     (setf hpg/root-hyperdrive (h/url-hyperdrive public-key))
     (hpg/load))))

;; TODO: Add menu bar interface.

(defun hpg/menu-bar (event)
  "Pop up `hyperdrive-peer-graph' menu bar."
  (interactive "e")
  (pcase (cadadr event)  ;; Image id from image map
    ((and (rx (group (= 52 alphanumeric))) public-key)
     (let ((hyperdrive (h/url-hyperdrive public-key)))
       (popup-menu
        `("Hyperdrive peer graph"
          ["Open hyperdrive"
           (lambda ()
             (interactive)
             (h/open (he//create :hyperdrive ,hyperdrive :path "/")))]))))))

;;;; Transient UI

;;;###autoload (autoload 'hyperdrive-peer-graph-menu "hyperdrive-peer-graph" nil t)
(transient-define-prefix hyperdrive-peer-graph-menu
  (topic hyperdrive sources-max-hops blockers-max-hops)
  "Show menu for HYPERDRIVE peer graph."
  ;; TODO: Update info manual link
  :info-manual "(hyperdrive)"
  :refresh-suffixes t
  ["Hyperdrive peer graph"
   :pad-keys t
   ;; TODO: When changing `hpg/root-hyperdrive', reset local variables to default values?
   ("r" hpg/set-root-hyperdrive)
   ("t" hpg/set-topic)
   ("g" "Reload" hpg/reload)]
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
  (h/peer-graph topic hyperdrive sources-max-hops blockers-max-hops)
  (transient-setup 'hyperdrive-peer-graph-menu nil nil :scope hyperdrive))

(transient-define-suffix hpg/set-root-hyperdrive ()
  :transient t
  :description
  (lambda ()
    (format "Root: %s" (if hpg/root-hyperdrive
                           (h//format hpg/root-hyperdrive)
                         (propertize "unset" 'face))))
  (interactive)
  (setf hpg/root-hyperdrive (h/read-hyperdrive :default hpg/root-hyperdrive))
  (hpg/load))

(transient-define-suffix hpg/set-topic ()
  :transient t
  :description
  (lambda ()
    (format "Topic: %s"
            (if hpg/topic
                (propertize hpg/topic 'face 'transient-argument)
              (propertize "Default" 'face 'transient-inactive-value))))
  (interactive)
  (setf hpg/topic (hpg/read-topic))
  (hpg/load))

(transient-define-suffix hpg/set-sources-max-hops ()
  :transient t
  :description
  (lambda ()
    (format (concat "Max hops for sources: "
                    (propertize "%d" 'face 'transient-argument))
            hpg/sources-max-hops))
  (interactive)
  (setf hpg/sources-max-hops
        (read-number "Max hops for sources: " hpg/sources-max-hops))
  (hpg/load))

(transient-define-suffix hpg/set-blockers-max-hops ()
  :transient t
  :description
  (lambda ()
    (format (concat "Max hops for blockers: "
                    (propertize "%d" 'face 'transient-argument))
            hpg/blockers-max-hops))
  (interactive)
  (setf hpg/blockers-max-hops
        (read-number "Max hops for blockers: " hpg/blockers-max-hops))
  (hpg/load))

(transient-define-suffix hpg/reload ()
  :inapt-if-not #'hpg/loaded-relations
  :transient t
  (interactive)
  (hpg/load))

(transient-define-suffix hpg/set-shortest-path-p ()
  :transient t
  :description (lambda ()
                 (format "Shortest paths only: %s"
                         (if hpg/shortest-path-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/shortest-path-p)
  (when-let ((buffer-window (get-buffer-window hpg/buffer-name)))
    (hpg/display-graph)))

(transient-define-suffix hpg/set-show-sources-p ()
  :transient t
  :description (lambda ()
                 (format "Show sources: %s"
                         (if hpg/show-sources-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-sources-p)
  (when-let ((buffer-window (get-buffer-window hpg/buffer-name)))
    (hpg/display-graph)))

(transient-define-suffix hpg/set-show-blockers-p ()
  :transient t
  :description (lambda ()
                 (format "Show blockers: %s"
                         (if hpg/show-blockers-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-blockers-p)
  (when-let ((buffer-window (get-buffer-window hpg/buffer-name)))
    (hpg/display-graph)))

(transient-define-suffix hpg/set-show-blocked-p ()
  :transient t
  :description (lambda ()
                 (format "Show blocked: %s"
                         (if hpg/show-blocked-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-value))))
  (interactive)
  (cl-callf not hpg/show-blocked-p)
  (when-let ((buffer-window (get-buffer-window hpg/buffer-name)))
    (hpg/display-graph)))

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
  (when-let ((buffer-window (get-buffer-window hpg/buffer-name)))
    (hpg/display-graph)))

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
  (when-let ((buffer-window (get-buffer-window hpg/buffer-name)))
    (hpg/display-graph)))

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
  (when-let ((buffer-window (get-buffer-window hpg/buffer-name)))
    (hpg/display-graph)))

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
