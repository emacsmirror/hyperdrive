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

;;;; Variables:

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
