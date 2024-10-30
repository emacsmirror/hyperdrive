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

(defvar hsg/root nil)
(defvar hsg/topic nil)
(defvar hsg/merge-relations nil)

;; TODO: Make default max hops customizable
(defvar hsg/sources-max-hops 3)
(defvar hsg/blockers-max-hops 3)

(defvar hsg/shortest-path-p t)

(defcustom hsg/buffer-name "*hyperdrive-social-graph-view*"
  "Buffer name to show social graph."
  :type 'string)

(defcustom hsg/default-topic "_default"
  "Special topic name used as a fallback when no topic is specified.")

;;;###autoload (autoload 'hyperdrive-social-graph "hyperdrive-social-graph" nil t)
(transient-define-prefix hyperdrive-social-graph (topic hyperdrive)
  "Show menu for HYPERDRIVE social graph."
  ;; TODO: Update info manual link
  :info-manual "(hyperdrive)"
  :refresh-suffixes t
  ["Hyperdrive social graph"
   :pad-keys t
   ("s" hsg/set-shortest-path)
   ("g" "Go (reload)" hsg/view)]
  (interactive (list (hsg/read-topic)
                     (h//context-hyperdrive :force-prompt current-prefix-arg)))
  ;; TODO: Add prefix to change root
  (setf hsg/root (h/public-key hyperdrive))
  (setf hsg/merge-relations
        (hsg/merge-relations
         (h/public-key hyperdrive)
         topic
         :sources-max-hops hsg/sources-max-hops
         :blockers-max-hops hsg/blockers-max-hops
         :finally (lambda (merge-relations)
                    (setf hsg/merge-relations merge-relations)
                    ;; TODO: Make h/fill-metadata async and request in a queue.
                    ;; (maphash (lambda (id _)
                    ;;            (h/fill-metadata (he/hyperdrive (h/url-entry))))
                    ;;          hsg/merge-relations)
                    (hsg/refresh))))
  (transient-setup 'hyperdrive-social-graph nil nil :scope hyperdrive))

(transient-define-suffix hsg/view ()
  :inapt-if-not (lambda () (and hsg/merge-relations
                                (not (processp hsg/merge-relations))))
  :transient t
  (interactive)
  (h/fons-view (hsg/filter hsg/merge-relations) hsg/root
               :label-fun #'hsg/hop-format-fun :buffer hsg/buffer-name))

(transient-define-suffix hsg/set-shortest-path ()
  :transient t
  :description (lambda ()
                 (format "Shortest path: %s"
                         (if hsg/shortest-path-p
                             (propertize "yes" 'face 'transient-argument)
                           (propertize "no" 'face 'transient-inactive-suffix))))
  (interactive)
  (cl-callf not hsg/shortest-path-p)
  (when-let ((buffer-window (get-buffer-window hsg/buffer-name)))
    (hsg/view)))

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
  (when hsg/shortest-path-p
    ;; Apply shortest-path before narrowing
    (cl-callf fons-filter-shortest-path merge-relations))
  (when hsg/narrow-to-p
    (cl-callf2 fons-filter-narrow-to
        (mapcar #'h/public-key hsg/narrow-hyperdrives) merge-relations))
  merge-relations)

(defun hsg/refresh ()
  "Refresh `hyperdrive-social-graph' if it's open."
  (when (transient-active-prefix 'hyperdrive-social-graph)
    (transient--refresh-transient)))

(defvar hsg/topic-history nil
  "Minibuffer history of `hyperdrive-social-graph-read-topic'.")

(defun hsg/read-topic ()
  "Read topic string.
Blank string defaults to `hyperdrive-social-graph-default-topic'."
  (let ((topic (read-string "Topic (leave blank for default topic): " nil hsg/topic-history)))
    (when (string-blank-p topic)
      (setf topic hsg/default-topic))
    topic))

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
