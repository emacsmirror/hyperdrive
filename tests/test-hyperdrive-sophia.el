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
     (cl-labels ((sophia-relations-for (peer)
                   (cdar (map-filter (lambda (from data)
                                       (equal from peer))
                                     sophia-relations))))
       ,@body)))

(sophia-test
  (sophia-relations-for "alice"))
(("tofu" #s(sophia-relation "alice" "carole" 0.8)
  #s(sophia-relation "alice" "bob" 0.25)))





(ert-deftest sophia-paths-alice-tofu-2-hop ()
  "Return alice's paths for \"tofu\" up to 2 hops away."
  (sophia-test 
    (let* ((from-alice (sophia-relations-for "alice"))
           (about-tofu (map-elt from-alice "tofu"))
           (1-hop-paths (cl-loop for relation in from-alice
                                 collect (make-sophia-path
                                          :score (sophia-relation-score relation)
                                          :relations (list relation))))
           (2-hop-paths (cl-loop for relation in from-alice
                                 collect (make-sophia-path
                                          :score (sophia-relation-score relation)
                                          :relations (list relation))))
           (paths (append 1-hop-paths 2-hop-paths)))
      (should
       (seq-set-equal-p
        paths
        (list (make-sophia-path
               :score 0.25
               :relations (list (make-sophia-relation :from "alice" :to "bob" :score 0.25)))
              (make-sophia-path
               :score 0.8
               :relations (list (make-sophia-relation :from "alice" :to "carole" :score 0.8)))
              (make-sophia-path
               :score 0.64
               :relations (list (make-sophia-relation :from "alice" :to "carole" :score 0.8)
                                (make-sophia-relation :from "carole" :to "david" :score 0.8)))
              (make-sophia-path
               :score 0.4
               :relations (list (make-sophia-relation :from "alice" :to "carole" :score 0.8)
                                (make-sophia-relation :from "carole" :to "eve" :score 0.5)))))))))

(ert-deftest sophia-paths-alice-tofu-1-hop ()
  "Return alice's paths for \"tofu\" up to 1 hop away."
  (sophia-test 
    (let* ((from-alice (sophia-relations-for "alice"))
           (about-tofu (map-elt from-alice "tofu"))
           (paths (cl-loop for relation in about-tofu
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
