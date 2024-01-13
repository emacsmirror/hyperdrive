;;; hyperdrive-sophia.el ---      -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'map)

;; TODO: Naming is hard.  (This struct doesn't really describe the relationship
;; between two people; it's one-directional.)
(cl-defstruct sophia-relation
  from to score)

(cl-defstruct sophia-path relations score)

(defun sophia-add-relation (from to score topic table)
  (let ((relation (make-sophia-relation :from from :to to :score score)))
    (push relation (map-elt (map-elt table
                                     (sophia-relation-from relation))
                            topic))))

(cl-defun sophia-paths (from topic &key (max-hops 3))
  "Return paths from FROM up to MAX-HOPS in RELATIONS about TOPIC."
  (let* ((relations (map-elt (sophia-relations from) topic))
         (paths (cl-loop for relation in relations
                         collect (make-sophia-path
                                  :score (sophia-relation-score relation)
                                  :relations (list relation)))))
    (unless (zerop (cl-decf max-hops))
      ;; Add hops up to the limit.
      (dolist (path paths)
        (let* ((last-relation (car (last (sophia-path-relations path))))
               (last-to (sophia-relation-to last-relation))
               (new-paths (sophia-paths last-to topic :max-hops max-hops)))
          (dolist (new-path new-paths)
            (let ((duplicate-path (copy-sequence path)))
              (cl-callf append (sophia-path-relations duplicate-path)
                (sophia-path-relations new-path))
              (setf (sophia-path-score duplicate-path) (sophia-score duplicate-path))
              (push duplicate-path paths))))))
    paths))

(defun sophia-score (path)
  (cl-reduce #'* (sophia-path-relations path) :key #'sophia-relation-score))

(provide 'hyperdrive-sophia)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hs/"  . "hyperdrive-sophia-")
;;   ("h/"   . "hyperdrive-"))
;; End:
