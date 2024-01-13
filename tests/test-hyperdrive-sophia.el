(require 'ert)

(require 'map)
(require 'seq)

(require 'hyperdrive-sophia)

(defvar test-hyperdrive-sophia-relations (make-hash-table :test 'equal))

(defmacro sophia-test (&rest body)
  (declare (indent defun) (debug (def-form)))
  `(progn
     (clrhash test-hyperdrive-sophia-relations)
     (sophia-add-relation "alice" "bob" 0.25 "tofu" test-hyperdrive-sophia-relations)
     (sophia-add-relation "alice" "carole" 0.8 "tofu" test-hyperdrive-sophia-relations)
     (sophia-add-relation "carole" "david" 0.8 "tofu" test-hyperdrive-sophia-relations)
     (sophia-add-relation "carole" "eve" 0.5 "tofu" test-hyperdrive-sophia-relations)
     (sophia-add-relation "david" "eve" 0.8 "tofu" test-hyperdrive-sophia-relations)
     (cl-letf (((symbol-function 'sophia-relations)
                (lambda (peer-name)
                  (cdar (map-filter (lambda (from data)
                                      (equal from peer-name))
                                    test-hyperdrive-sophia-relations)))))
       ,@body)))

;; (sophia-test
;;   (sophia-relations-for "alice"))

(ert-deftest sophia-paths-alice-tofu-3-hops ()
  "Return alice's paths for \"tofu\" up to 3 hops away."
  (sophia-test 
    (let* ((paths (sophia-paths "alice" "tofu" :max-hops 3)))
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
               :score 0.6400000000000001
               :relations (list (make-sophia-relation :from "alice" :to "carole" :score 0.8)
                                (make-sophia-relation :from "carole" :to "david" :score 0.8)))
              (make-sophia-path
               :score 0.4
               :relations (list (make-sophia-relation :from "alice" :to "carole" :score 0.8)
                                (make-sophia-relation :from "carole" :to "eve" :score 0.5)))
              (make-sophia-path
               :score 0.5120000000000001
               :relations (list (make-sophia-relation :from "alice" :to "carole" :score 0.8)
                                (make-sophia-relation :from "carole" :to "david" :score 0.8)
                                (make-sophia-relation :from "david" :to "eve" :score 0.8)))))))))

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hs/"  . "hyperdrive-sophia-")
;;   ("h/"   . "hyperdrive-"))
;; End:
