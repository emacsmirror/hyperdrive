(require 'ert)

(require 'map)
(require 'seq)

(require 'hyperdrive-fons)

(defvar test-hyperdrive-fons-hops (make-hash-table :test 'equal))

(eval-and-compile
  (defvar test-hyperdrive-fons-default-hops-fn
    (lambda ()
      (fons-add-hop "alice" "bob" 0.25 "tofu" test-hyperdrive-fons-hops)
      (fons-add-hop "alice" "carole" 0.8 "tofu" test-hyperdrive-fons-hops)
      (fons-add-hop "carole" "david" 0.8 "tofu" test-hyperdrive-fons-hops)
      (fons-add-hop "carole" "eve" 0.5 "tofu" test-hyperdrive-fons-hops)
      (fons-add-hop "david" "eve" 0.8 "tofu" test-hyperdrive-fons-hops))))

(cl-defmacro fons-test ((&optional hops-fn) &rest body)
  (declare (indent defun) (debug (def-form)))
  `(progn
     (clrhash test-hyperdrive-fons-hops)
     (funcall ,(if hops-fn
                   hops-fn
                 test-hyperdrive-fons-default-hops-fn))
     (cl-letf (((symbol-function 'fons-hops)
                (lambda (peer-name)
                  (cdar (map-filter (lambda (from data)
                                      (equal from peer-name))
                                    test-hyperdrive-fons-hops)))))
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
               :hops (list (make-fons-hop
                            :from "alice" :to "bob" :score 0.25)))
              (make-fons-path
               :score 0.8
               :hops (list (make-fons-hop
                            :from "alice" :to "carole" :score 0.8)))
              (make-fons-path
               :score 0.6400000000000001
               :hops (list (make-fons-hop
                            :from "alice" :to "carole" :score 0.8)
                           (make-fons-hop
                            :from "carole" :to "david" :score 0.8)))
              (make-fons-path
               :score 0.4
               :hops (list (make-fons-hop
                            :from "alice" :to "carole" :score 0.8)
                           (make-fons-hop
                            :from "carole" :to "eve" :score 0.5)))
              (make-fons-path
               :score 0.5120000000000001
               :hops (list (make-fons-hop
                            :from "alice" :to "carole" :score 0.8)
                           (make-fons-hop
                            :from "carole" :to "david" :score 0.8)
                           (make-fons-hop
                            :from "david" :to "eve" :score 0.8)))))))))

(ert-deftest fons-filter-short-circuits ()
  "Hops of sources with scores below the threshold are skipped."
  ;; NEXT: Write this test to ensure that the paths from alice to georgie and to
  ;; hobart are filtered out.
  (fons-test ((lambda ()
                (funcall test-hyperdrive-fons-default-hops-fn)
                (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops)))
    (let (fons-hops-called-with)
      (cl-letf* ((orig-fn (symbol-function 'fons-hops))
                 ((symbol-function 'fons-hops)
                  (lambda (&rest args)
                    (push args fons-hops-called-with)
                    (apply orig-fn args))))
        (fons-paths "alice" "tofu" :max-hops 3)
        (should (seq-contains-p fons-hops-called-with '("frank")))
        (should-not (seq-contains-p fons-hops-called-with '("georgie")))))))

;; TODO: Add test that aggregates paths into a score (where paths A>B>D and
;; A>C>D are each below the threshold but, together, A...D is above
;; it). (Consider expressing the A...D relation as a "relation" struct or
;; something like that, which may contain multiple independent paths between A
;; and D.)

(ert-deftest fons-aggregate-paths ()
  "Multiple paths between A and D are aggregated into a relation with a certain score."
  (fons-test ()
    (let* ((tofu-paths-from-alice-3-hops (fons-paths "alice" "tofu"))
           (paths-to-eve (cl-remove-if-not
                          (lambda (path)
                            (fons-path-to-p "eve" path))
                          tofu-paths-from-alice-3-hops))
           (relation-to-eve (make-fons-relation
                             :from "alice" :to "eve" :paths paths-to-eve)))
      (should (fons-relation-p relation-to-eve))
      (should (equal "alice" (fons-relation-from relation-to-eve)))
      (should (equal "eve" (fons-relation-to relation-to-eve)))
      (should (numberp (fons-score-relation relation-to-eve)))
      ;; Filter paths below score?
      (should (length= 0 (fons-relation-paths relation-to-eve))))))

(ert-deftest fons-path-to-p ()
  "Returns non-nil if PATH ends in TO."
  (should (fons-path-to-p "bob"
                          (make-fons-path
                           :hops (list (make-fons-hop
                                        :from "alice" :to "bob")))))
  (should (fons-path-to-p "eve"
                          (make-fons-path
                           :hops (list (make-fons-hop
                                        :from "alice" :to "carole")
                                       (make-fons-hop
                                        :from "carole" :to "eve")))))
  (should-not (fons-path-to-p "carole"
                              (make-fons-path
                               :hops (list (make-fons-hop
                                            :from "alice" :to "carole")
                                           (make-fons-hop
                                            :from "carole" :to "eve"))))))

  ;; Local Variables:
  ;; read-symbol-shorthands: (
  ;;   ("he//" . "hyperdrive-entry--")
  ;;   ("he/"  . "hyperdrive-entry-")
  ;;   ("h//"  . "hyperdrive--")
  ;;   ("hf/"  . "hyperdrive-fons-")
  ;;   ("h/"   . "hyperdrive-"))
  ;; End:
