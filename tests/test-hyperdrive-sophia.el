(require 'ert)

(require 'hyperdrive-sophia)

(defmacro sophia-test (&rest body)
  (declare (indent defun) (debug (def-form)))
  `(progn
     (clrhash sophia-relations)
     (sophia-add-relation "alice" "bob" 0.25 "tofu" sophia-relations)
     (sophia-add-relation "alice" "carole" 0.8 "tofu" sophia-relations)
     (sophia-add-relation "carole" "david" 0.8 "tofu" sophia-relations)
     (sophia-add-relation "carole" "eve" 0.5 "tofu" sophia-relations)
     (sophia-add-relation "david" "eve" 0.8 "tofu" sophia-relations)
     ,@body))

(ert-deftest sophia-paths-alice-tofu-1-hop ()
  "Return alice's paths for \"tofu\" up to 1 hop away."
  (sophia-test 
    (let* ((about-tofu (map-elt sophia-relations "tofu"))
           (from-alice (map-elt about-tofu "alice"))
           (paths (cl-loop for relation in from-alice
                           collect (make-sophia-path
                                    :score (sophia-relation-score relation)
                                    :relations (list relation)))))
      (should
       (seq-set-equal-p
        paths
        (list (make-sophia-path
               :score 0.25
               :relations (list (make-sophia-relation :from "alice" :to "bob" :score 0.25)))
              (make-sophia-path
               :score 0.8
               :relations (list (make-sophia-relation :from "alice" :to "carole" :score 0.8)))))))))
