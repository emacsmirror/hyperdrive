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

(defconst hsg/data-filename "/.well-known/social-graph.json"
  "Hyperdrive filename to search for social graph data.")

(defun hsg/hops-fn (from topic then)
  "Asynchronously get hops from FROM about TOPIC.
Call THEN with a list of `fons-hop' structs.."
  (let* ((from-entry (h/url-entry from))
         ;; (from-user (make-fons-user :id from))
         (_ (setf (he/path from-entry) hsg/data-filename))
         hops)
    (he/api 'get from-entry :noquery t
      ;; Despite error, always call THEN so `pending' gets decremented.
      :then (lambda (response)
              (condition-case err
                  (let* ((parsed
                          ;; TODO: When plz adds :as 'response-with-buffer, use that.
                          (json-parse-string (plz-response-body response)
                                             :array-type 'list))
                         (tos (map-elt (map-elt parsed "sources") topic))
                         (hops (mapcar (lambda (to)
                                         (make-fons-hop :from from :to to))
                                       tos)))
                    ;; (setf (fons-user-sources from-user) tos)
                    ;; (message "HOPS: %S" hops)
                    (funcall then hops))
                (json-error
                 (h/message "Error parsing social graph data: %s" (he/url from-entry))
                 (funcall then nil))))
      :else (lambda (plz-error)
              (pcase (plz-response-status (plz-error-response plz-error))
                ;; FIXME: If plz-error is a curl-error, this block will fail.
                (404
                 (h/message "No social graph data found: %s" (he/url from-entry))
                 (funcall then nil))
                (_
                 ;; TODO: Put error in another buffer.  Check error 500 for malformed URLs?
                 (h/message "Error getting social graph data: %s" (he/url from-entry) plz-error)
                 (funcall then nil)))))))

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
