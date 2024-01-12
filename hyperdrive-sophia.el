(defvar sophia-relations (make-hash-table :test #'equal))

(cl-defstruct sophia-relation
  from to score)

(cl-defstruct sophia-path relations score)

(defun sophia-add-relation (from to score topic table)
  (let ((relation (make-sophia-relation :from from :to to :score score)))
    (push relation (map-elt (map-elt table topic) (sophia-relation-from relation)))))

(provide 'hyperdrive-sophia)
