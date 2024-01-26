;;; hyperdrive-fons.el ---      -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'map)

;; TODO: Naming is hard.  (This struct doesn't really describe the relationship
;; between two people; it's one-directional.)
(cl-defstruct fons-relation
  from to score)

(cl-defstruct fons-path relations score)

(defun fons-add-relation (from to score topic table)
  (let ((relation (make-fons-relation :from from :to to :score score)))
    (push relation (map-elt (map-elt table
                                     (fons-relation-from relation))
                            topic))))

(defun fons-relations (_from)
  "Return relations from user FROM."
  (error "Not yet implemented (bound in tests)"))

(cl-defun fons-paths (from topic &key (max-hops 3))
  "Return paths from FROM up to MAX-HOPS in RELATIONS about TOPIC."
  (let* ((relations (map-elt (fons-relations from) topic))
         (paths (cl-loop for relation in relations
                         collect (make-fons-path
                                  :score (fons-relation-score relation)
                                  :relations (list relation)))))
    ;; NOTE: At this point, `paths' only has one-hop paths.
    (unless (zerop (cl-decf max-hops))
      ;; Add hops up to the limit.
      (dolist (path paths)
        (let* ((last-relation (car (last (fons-path-relations path))))
               (last-to (fons-relation-to last-relation))
               (new-paths (fons-paths last-to topic :max-hops max-hops)))
          (dolist (new-path new-paths)
            (let ((duplicate-path (copy-sequence path)))
              (cl-callf append (fons-path-relations duplicate-path)
                (fons-path-relations new-path))
              (setf (fons-path-score duplicate-path) (fons-score duplicate-path))
              (push duplicate-path paths))))))
    paths))

(defun fons-score (path)
  (cl-reduce #'* (fons-path-relations path) :key #'fons-relation-score))

;; (cl-defun fons-filter-to (to paths)
;;   "Return PATHS that end at TO."
;;   (cl-remove-if-not
;;    (lambda (path)
;;      (equal to (fons-relation-to (car (last (fons-path-relations path))))))
;;    paths))

;; (fons-filter-to "eve" paths)

;; (defcustom fons-aggregate-fn #'fons-aggregate-score-default
;;   "Path aggregation function."
;;   :type 'function
;;   :group 'hyperdrive)

;; (defun fons-aggregate-score (paths)
;;   "Return aggregate score of PATHS."
;;   (funcall fons-aggregate-fn paths))

;; (defun fons-aggregate-score-default (paths)
;;   "Return the aggregate score for PATHS."
;;   ;; TODO: Consider using a weighted mean based on path length.
;;   (cl-loop for path in paths
;;            maximizing (fons-path-score path)))

(provide 'hyperdrive-fons)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hf/"  . "hyperdrive-fons-")
;;   ("h/"   . "hyperdrive-"))
;; End:
