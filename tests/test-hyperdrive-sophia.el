(require 'ert)

(require 'map)
(require 'seq)

(require 'hyperdrive-fons)

(defvar test-hyperdrive-fons-relations (make-hash-table :test 'equal))

(eval-and-compile
  (defvar test-hyperdrive-fons-default-relations-fn
    (lambda ()
      (fons-add-relation "alice" "bob" 0.25 "tofu" test-hyperdrive-fons-relations)
      (fons-add-relation "alice" "carole" 0.8 "tofu" test-hyperdrive-fons-relations)
      (fons-add-relation "carole" "david" 0.8 "tofu" test-hyperdrive-fons-relations)
      (fons-add-relation "carole" "eve" 0.5 "tofu" test-hyperdrive-fons-relations)
      (fons-add-relation "david" "eve" 0.8 "tofu" test-hyperdrive-fons-relations))))

(cl-defmacro fons-test ((&optional relations-fn) &rest body)
  (declare (indent defun) (debug (def-form)))
  `(progn
     (clrhash test-hyperdrive-fons-relations)
     (funcall ,(if relations-fn
                   relations-fn
                 test-hyperdrive-fons-default-relations-fn))
     (cl-letf (((symbol-function 'fons-relations)
                (lambda (peer-name)
                  (cdar (map-filter (lambda (from data)
                                      (equal from peer-name))
                                    test-hyperdrive-fons-relations)))))
       ,@body)))

(ert-deftest fons-paths-alice-tofu-3-hops ()
  "Return alice's paths for \"tofu\" up to 3 hops away."
  (fons-test ()
    (let* ((paths (fons-paths "alice" "tofu" :max-hops 3)))
      (should
       (seq-set-equal-p
        paths
        (list (make-fons-path
               :score 0.25
               :relations (list (make-fons-relation :from "alice" :to "bob" :score 0.25)))
              (make-fons-path
               :score 0.8
               :relations (list (make-fons-relation :from "alice" :to "carole" :score 0.8)))
              (make-fons-path
               :score 0.6400000000000001
               :relations (list (make-fons-relation :from "alice" :to "carole" :score 0.8)
                                (make-fons-relation :from "carole" :to "david" :score 0.8)))
              (make-fons-path
               :score 0.4
               :relations (list (make-fons-relation :from "alice" :to "carole" :score 0.8)
                                (make-fons-relation :from "carole" :to "eve" :score 0.5)))
              (make-fons-path
               :score 0.5120000000000001
               :relations (list (make-fons-relation :from "alice" :to "carole" :score 0.8)
                                (make-fons-relation :from "carole" :to "david" :score 0.8)
                                (make-fons-relation :from "david" :to "eve" :score 0.8)))))))))

(ert-deftest fons-filter-short-circuits ()
  ;; NEXT: Write this test to ensure that the paths from alice to georgie and to
  ;; hobart are filtered out.
  (fons-test ((lambda ()
                (funcall test-hyperdrive-fons-default-relations-fn)
                (fons-add-relation "alice" "frank" 1 "tofu" test-hyperdrive-fons-relations)
                (fons-add-relation "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-relations)
                (fons-add-relation "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-relations)))
    (let (fons-relations-called-with)
      (cl-letf* ((orig-fn (symbol-function 'fons-relations))
                 ((symbol-function 'fons-relations)
                  (lambda (&rest args)
                    (push args fons-relations-called-with)
                    (apply orig-fn args))))
        (fons-paths "alice" "tofu" :max-hops 3)
        (should (seq-contains-p fons-relations-called-with '("frank")))
        (should-not (seq-contains-p fons-relations-called-with '("georgie")))))))

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hf/"  . "hyperdrive-fons-")
;;   ("h/"   . "hyperdrive-"))
;; End:
