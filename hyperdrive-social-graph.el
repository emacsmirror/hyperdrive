;;; hyperdrive-social-graph.el --- Visualize fons relations  -*- lexical-binding: t; -*-

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

;; This library implements a graphviz-based view for `fons' relations data.

;; Some of the code is borrowed from `org-graph-view':
;; https://github.com/alphapapa/org-graph-view, which is GPLv3+ licensed.

;;; Code:

(require 'hyperdrive-fons-view)

;;;; Variables:

(defgroup hyperdrive-social-graph nil
  "Customization of the hyperdrive social graph."
  :group 'hyperdrive)

(defconst hsg/data-filename "/.well-known/social-graph.json"
  "Hyperdrive filename to search for social graph data.")

;;;; Functions:

(defun hsg/data (public-key topic &key then)
  "Load social graph data for PUBLIC-KEY on TOPIC then call THEN.
THEN will be called with the parsed JSON hash table as its sole
argument.  If error, demote it and call THEN with nil argument."
  ;; TODO: Add a queue limit.
  (let ((entry (h/url-entry public-key)))
    (setf (he/path entry) hsg/data-filename)
    (he/api 'get entry :noquery t
      ;; Despite error, always call THEN so `pending' gets decremented.
      :then (lambda (response)
              (condition-case err
                  ;; TODO: When plz adds :as 'response-with-buffer, use that.
                  (funcall then (map-elt (json-parse-string
                                          (plz-response-body response)
                                          :array-type 'list)
                                         topic))
                (json-error
                 (h/message "Error parsing social graph data: %s\n%S"
                            (he/url entry) err)
                 (funcall then nil))))
      :else (lambda (plz-error)
              (pcase (plz-response-status (plz-error-response plz-error))
                ;; FIXME: If plz-error is a curl-error, this block will fail.
                (404
                 (h/message "No social graph data found: %s" (he/url entry))
                 (funcall then nil))
                (_
                 ;; TODO: Put error in another buffer.  Check error 500 for malformed URLs?
                 (h/message "Error getting social graph data: %s" (he/url entry) plz-error)
                 (funcall then nil)))))))

(defun hsg/hops-fn (from topic then)
  "Asynchronously get hops from FROM about TOPIC.
Call THEN with a list of TOs."
  (hsg/data from topic :then then))

(cl-defun hsg/blocked-fn (blocker then)
  "Asynchronously get blocks from BLOCKER.
Call THEN with a list of block IDs."
  (hsg/data blocker "_blocked" :then then))

(defun hsg/hop-format-fun (hop)
  "Return display string for HOP."
  (h//format (he/hyperdrive (h/url-entry hop))))

;;;; Transient UI

(defvar hsg/root-hyperdrive nil)
(defvar hsg/topic nil)
(defvar hsg/merge-relations nil)

;; TODO: Make default max hops customizable
(defvar hsg/sources-max-hops 3)
(defvar hsg/blockers-max-hops 3)

(defvar hsg/show-sources-p t)
(defvar hsg/show-blockers-p t)
(defvar hsg/show-blocked-p t)

(defvar hsg/shortest-path-p t)
(defvar hsg/only-paths-to nil)

(defcustom hsg/buffer-name "*hyperdrive-social-graph-view*"
  "Buffer name to show social graph."
  :type 'string)

(defcustom hsg/default-topic "_default"
  "Special topic name used as a fallback when no topic is specified.")

(defun hyperdrive-social-graph (topic hyperdrive)
  "Show menu for HYPERDRIVE social graph."
  (interactive (list (hsg/context-topic :force-prompt current-prefix-arg)
                     (hsg/context-root-hyperdrive :force-prompt current-prefix-arg)))
  (if (and (equal topic hsg/topic)
           hsg/root-hyperdrive
           (h/equal-p hyperdrive hsg/root-hyperdrive)
           (hsg/loaded-merge-relations))
      (hsg/display-graph)
    (setf hsg/topic topic)
    (setf hsg/root-hyperdrive hyperdrive)
    (hsg/load)))

;;;###autoload (autoload 'hyperdrive-social-graph "hyperdrive-social-graph" nil t)
(transient-define-prefix hyperdrive-social-graph-menu (topic hyperdrive)
  "Show menu for HYPERDRIVE social graph."
  ;; TODO: Update info manual link
  :info-manual "(hyperdrive)"
  :refresh-suffixes t
  ["Hyperdrive social graph"
   :pad-keys t
   ;; TODO: When changing `hsg/root-hyperdrive', reset local variables to default values?
   ("r" hsg/set-root-hyperdrive)
   ("t" hsg/set-topic)
   ("g" "Reload" hsg/reload)]
  ["Only paths to"
   (:info #'hsg/format-only-paths-to :format "%d")
   ("n a" "Add" hsg/only-paths-to-add)
   ("n d" "Delete" hsg/only-paths-to-delete)]
  [["Sources"
    ("s s" hsg/set-show-sources-p)
    ("s m" hsg/set-sources-max-hops)]
   ["Blockers"
    ("b s" hsg/set-show-blockers-p)
    ("b m" hsg/set-blockers-max-hops)]
   ["Blocked"
    ("x s" hsg/set-show-blocked-p)]]
  ["Options"
   ("S" hsg/set-shortest-path-p)]

  (interactive (list (hsg/context-topic :force-prompt current-prefix-arg)
                     (hsg/context-root-hyperdrive :force-prompt current-prefix-arg)))
  (h/social-graph topic hyperdrive)
  (transient-setup 'hyperdrive-social-graph-menu nil nil :scope hyperdrive))

(defun hsg/load ()
  "Load `hsg/merge-relations' and redisplay graph."
  (setf hsg/merge-relations
        (hsg/merge-relations
         (h/public-key hsg/root-hyperdrive)
         (or hsg/topic hsg/default-topic)
         :sources-max-hops hsg/sources-max-hops
         :blockers-max-hops hsg/blockers-max-hops
         :finally (lambda (merge-relations)
                    (setf hsg/merge-relations merge-relations)
                    (hsg/display-graph)
                    ;; TODO: Make h/fill-metadata async and request in a queue.
                    ;; (maphash (lambda (id _)
                    ;;            (h/fill-metadata (he/hyperdrive (h/url-entry))))
                    ;;          hsg/merge-relations)
                    (hsg/refresh-menu))))
  (hsg/display-loading-buffer))

(transient-define-suffix hsg/set-root-hyperdrive ()
  :transient t
  :description
  (lambda ()
    (format "Root: %s" (if hsg/root-hyperdrive
                           (h//format-hyperdrive hsg/root-hyperdrive)
                         (propertize "unset" 'face))))
  (interactive)
  (setf hsg/root-hyperdrive (h/read-hyperdrive :default hsg/root-hyperdrive))
  (hsg/load))

(transient-define-suffix hsg/set-topic ()
  :transient t
  :description
  (lambda ()
    (format "Topic: %s"
            (if hsg/topic
                (propertize hsg/topic 'face 'transient-argument)
              (propertize "Default" 'face 'transient-inactive-suffix))))
  (interactive)
  (setf hsg/topic (hsg/read-topic))
  (hsg/load))

(transient-define-suffix hsg/set-sources-max-hops ()
  :transient t
  :description
  (lambda ()
    (format (concat "Max hops for sources: "
                    (propertize "%d" 'face 'transient-argument))
            hsg/sources-max-hops))
  (interactive)
  (setf hsg/sources-max-hops
        (read-number "Max hops for sources: " hsg/sources-max-hops))
  (hsg/load))

(transient-define-suffix hsg/set-blockers-max-hops ()
  :transient t
  :description
  (lambda ()
    (format (concat "Max hops for blockers: "
                    (propertize "%d" 'face 'transient-argument))
            hsg/blockers-max-hops))
  (interactive)
  (setf hsg/blockers-max-hops
        (read-number "Max hops for blockers: " hsg/blockers-max-hops))
  (hsg/load))

(transient-define-suffix hsg/reload ()
  :inapt-if-not #'hsg/loaded-merge-relations
  :transient t
  (interactive)
  (hsg/load))

(defun hsg/display-loading-buffer ()
  "Open loading buffer for hyperdrive social graph."
  (with-current-buffer hsg/buffer-name
    (with-silent-modifications
      (erase-buffer)
      (insert "Loading hyperdrive social graph data...")
      (display-buffer (current-buffer)))))

;; TODO: Display full screen by default.
;; TODO: Accept display-action argument.  Default is to not pop to buffer.
(defun hsg/display-graph ()
  "Open buffer displaying hyperdrive social graph."
  (h/fons-view (hsg/filter hsg/merge-relations)
               (h/public-key hsg/root-hyperdrive)
               :label-fun #'hsg/hop-format-fun :buffer hsg/buffer-name))

(transient-define-suffix hsg/set-shortest-path-p ()
  :transient t
  :description (lambda ()
                 (format "Shortest path: %s"
                         (if hsg/shortest-path-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-suffix))))
  (interactive)
  (cl-callf not hsg/shortest-path-p)
  (when-let ((buffer-window (get-buffer-window hsg/buffer-name)))
    (hsg/display-graph)))

(transient-define-suffix hsg/set-show-sources-p ()
  :transient t
  :description (lambda ()
                 (format "Show sources: %s"
                         (if hsg/show-sources-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-suffix))))
  (interactive)
  (cl-callf not hsg/show-sources-p)
  (when-let ((buffer-window (get-buffer-window hsg/buffer-name)))
    (hsg/display-graph)))

(transient-define-suffix hsg/set-show-blockers-p ()
  :transient t
  :description (lambda ()
                 (format "Show blockers: %s"
                         (if hsg/show-blockers-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-suffix))))
  (interactive)
  (cl-callf not hsg/show-blockers-p)
  (when-let ((buffer-window (get-buffer-window hsg/buffer-name)))
    (hsg/display-graph)))

(transient-define-suffix hsg/set-show-blocked-p ()
  :transient t
  :description (lambda ()
                 (format "Show blocked: %s"
                         (if hsg/show-blocked-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-suffix))))
  (interactive)
  (cl-callf not hsg/show-blocked-p)
  (when-let ((buffer-window (get-buffer-window hsg/buffer-name)))
    (hsg/display-graph)))

(defun hsg/format-only-paths-to ()
  (string-join
   (mapcar (lambda (hyperdrive)
             (format "     - %s" (h//format hyperdrive)))
           hsg/only-paths-to)
   "\n"))

(transient-define-suffix hsg/only-paths-to-add (hyperdrive)
  "Add HYPERDRIVE to `hsg/only-paths-to' and reload.
Only drives not in `hsg/only-paths-to' are offered for completion."
  :transient t
  :inapt-if-not #'hsg/loaded-merge-relations
  (interactive
   (list (h/read-hyperdrive :predicate
           (lambda (hyperdrive)
             (unless (cl-member hyperdrive hsg/only-paths-to
                                :test #'h/equal-p)
               (catch 'break
                 (maphash (lambda (id _)
                            (when (string= (h/public-key hyperdrive) id)
                              (throw 'break t)))
                          hsg/merge-relations)))))))
  (push hyperdrive hsg/only-paths-to)
  (when-let ((buffer-window (get-buffer-window hsg/buffer-name)))
    (hsg/display-graph)))

(transient-define-suffix hsg/only-paths-to-delete (hyperdrive)
  "Delete HYPERDRIVE from `hsg/only-paths-to' and reload."
  :transient t
  :inapt-if-not #'hsg/loaded-merge-relations
  (interactive (list (h/read-hyperdrive :predicate
                       (lambda (hyperdrive)
                         (cl-member hyperdrive hsg/only-paths-to
                                    :test #'h/equal-p)))))
  (setf hsg/only-paths-to
        (cl-delete hyperdrive hsg/only-paths-to :test #'h/equal-p))
  (when-let ((buffer-window (get-buffer-window hsg/buffer-name)))
    (hsg/display-graph)))

(cl-defun hsg/merge-relations (root topic &key finally sources-max-hops blockers-max-hops)
  "Load merge-relations from ROOT about TOPIC and call FINALLY.
FINALLY should be a function which accepts a single argument, a
\\+`merge-relations' hash table, as in `fons-merge-relations'.
SOURCES-MAX-HOPS and BLOCKERS-MAX-HOPS are the maximum number of
hops to traverse for sources and blockers, respectively."
  (fons-relations
   root :hops-fn #'hsg/hops-fn :topic "_blockers" :max-hops blockers-max-hops
   :finally
   (lambda (blockers)
     (fons-blocked
      blockers :blocked-fn #'hsg/blocked-fn :finally
      (lambda (blocked)
        (fons-relations
         root :hops-fn #'hsg/hops-fn :topic topic
         :blocked blocked :max-hops sources-max-hops :finally
         (lambda (sources)
           (funcall finally
                    (fons-merge-relations sources blockers blocked)))))))))

(defun hsg/filter (merge-relations)
  "Return filtered MERGE-RELATIONS."
  ;; TODO: Make filters customizable
  (unless (and hsg/show-sources-p hsg/show-blockers-p hsg/show-blocked-p)
    (cl-callf fons-filter-to-types merge-relations
      :sourcesp hsg/show-sources-p
      :blockersp hsg/show-blockers-p
      :blockedp hsg/show-blocked-p))
  (when hsg/shortest-path-p
    (cl-callf fons-filter-shortest-path merge-relations))
  ;; Apply `hsg/only-paths-to' last
  (cl-callf2 fons-filter-only-paths-to
      (mapcar #'h/public-key hsg/only-paths-to) merge-relations)
  merge-relations)

(defun hsg/refresh-menu ()
  "Refresh `hyperdrive-social-graph-menu' if it's open."
  (when (transient-active-prefix 'hyperdrive-social-graph-menu)
    (transient--refresh-transient)))

(defvar hsg/topic-history nil
  "Minibuffer history of `hyperdrive-social-graph-read-topic'.")

(defun hsg/read-topic ()
  "Read topic string or nil with blank string."
  (let ((topic (read-string "Topic (leave blank for default topic): "
                            nil hsg/topic-history)))
    (and (not (string-blank-p topic))
         topic)))

(cl-defun hsg/context-topic (&key force-prompt)
  "Return `hyperdrive-social-graph-topic'.
With FORCE-PROMPT, or interactively with universal prefix
argument \\[universal-argument], always prompt.
Blank string defaults to `hyperdrive-social-graph-default-topic'."
  (if force-prompt (hsg/read-topic) hsg/topic))

(cl-defun hsg/context-root-hyperdrive (&key force-prompt)
  "Return `hyperdrive-social-graph-root-hyperdrive' or prompt for drive.
With FORCE-PROMPT, or interactively with universal prefix
argument \\[universal-argument], always prompt."
  (interactive (list current-prefix-arg))
  (if (and hsg/root-hyperdrive (not force-prompt))
      hsg/root-hyperdrive
    (h/read-hyperdrive :default (or (and h/current-entry
                                         (he/hyperdrive h/current-entry))
                                    hsg/root-hyperdrive))))

(defun hsg/loaded-merge-relations ()
  "Return `hyperdrive-social-graph-merge-relations' if loaded."
  (and (not (processp hsg/merge-relations))
       hsg/merge-relations))

;;; Footer:

(provide 'hyperdrive-social-graph)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hsg/"  . "hyperdrive-social-graph-")
;;   ("hsg//"  . "hyperdrive-social-graph--")
;;   ("hf/"  . "hyperdrive-fons-")
;;   ("h/"   . "hyperdrive-"))
;; End:

;;; hyperdrive-social-graph.el ends here
