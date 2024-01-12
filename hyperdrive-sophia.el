(cl-defstruct sophia-relation
  from to score)

(cl-defstruct sophia-path relations score)

(defun sophia-add-relation (from to score topic table)
  (let ((relation (make-sophia-relation :from from :to to :score score)))
    (push relation (map-elt (map-elt table
                                     (sophia-relation-from relation))
                            topic))))

(cl-defun sophia-paths (from topic relations &key (max-hops 3))
  "Return paths from FROM up to MAX-HOPS in RELATIONS about TOPIC."
  )

(provide 'hyperdrive-sophia)
