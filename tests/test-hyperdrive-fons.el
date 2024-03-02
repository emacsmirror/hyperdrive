;;; test-hyperdrive-fons.el ---      -*- lexical-binding: t; -*-

(require 'ert)

(require 'map)
(require 'seq)

(require 'hyperdrive-fons)
(require 'hyperdrive-fons-view)

(defvar test-hyperdrive-fons-hops (make-hash-table :test 'equal))

(defvar test-hyperdrive-fons-default-hops-fn
  (lambda ()
    (fons-add-hop "alice" "bob" 0.25 "tofu" test-hyperdrive-fons-hops)
    (fons-add-hop "alice" "carol" 0.8 "tofu" test-hyperdrive-fons-hops)
    (fons-add-hop "carol" "david" 0.8 "tofu" test-hyperdrive-fons-hops)
    (fons-add-hop "carol" "eve" 0.5 "tofu" test-hyperdrive-fons-hops)
    (fons-add-hop "david" "eve" 0.8 "tofu" test-hyperdrive-fons-hops)))

(cl-defmacro fons-test ((&optional hops-fn) &rest body)
  (declare (indent defun) (debug (([&optional lambda-expr]) def-body)))
  `(progn
     (clrhash test-hyperdrive-fons-hops)
     (funcall (or ,hops-fn
                  test-hyperdrive-fons-default-hops-fn))
     (cl-letf (((symbol-function 'fons-hops)
                (lambda (peer-name)
                  (cdar (map-filter (lambda (from _data)
                                      (equal from peer-name))
                                    test-hyperdrive-fons-hops)))))
       ,@body)))

(type-of (cdar (fons-test ()
                 (fons-hops "alice"))))

(ert-deftest fons-paths-alice-tofu-3-hops ()
  "Return alice's paths-from-alice for \"tofu\" up to 3 hops away."
  (fons-test ()
    (let* ((paths-from-alice (fons-paths "alice" "tofu" :max-hops 3)))
      (should
       (seq-set-equal-p
        (list (make-fons-path
               :score 0.25
               :hops (list (make-fons-hop
                            :from "alice" :to "bob" :score 0.25)))
              (make-fons-path
               :score 0.8
               :hops (list (make-fons-hop
                            :from "alice" :to "carol" :score 0.8)))
              (make-fons-path
               :score 0.6400000000000001
               :hops (list (make-fons-hop
                            :from "alice" :to "carol" :score 0.8)
                           (make-fons-hop
                            :from "carol" :to "david" :score 0.8)))
              (make-fons-path
               :score 0.4
               :hops (list (make-fons-hop
                            :from "alice" :to "carol" :score 0.8)
                           (make-fons-hop
                            :from "carol" :to "eve" :score 0.5)))
              (make-fons-path
               :score 0.5120000000000001
               :hops (list (make-fons-hop
                            :from "alice" :to "carol" :score 0.8)
                           (make-fons-hop
                            :from "carol" :to "david" :score 0.8)
                           (make-fons-hop
                            :from "david" :to "eve" :score 0.8))))
        paths-from-alice)))))

;; TODO: Test that uses the relation score in fons-paths (i.e. for
;; short-circuiting computations).

(ert-deftest fons-filter-short-circuits ()
  "Hops of sources with scores below the threshold are skipped."
  ;; NEXT: Write this test to ensure that the paths-from-alice from alice to georgie and to
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

(ert-deftest fons-path-to-p ()
  "Returns non-nil if PATH ends in TO."
  (should (fons-path-to-p "bob"
                          (make-fons-path
                           :hops (list (make-fons-hop
                                        :from "alice" :to "bob")))))
  (should (fons-path-to-p "eve"
                          (make-fons-path
                           :hops (list (make-fons-hop
                                        :from "alice" :to "carol")
                                       (make-fons-hop
                                        :from "carol" :to "eve")))))
  (should-not (fons-path-to-p "carol"
                              (make-fons-path
                               :hops (list (make-fons-hop
                                            :from "alice" :to "carol")
                                           (make-fons-hop
                                            :from "carol" :to "eve"))))))

(ert-deftest fons-path-circular-p ()
  "Returns non-nil if PATH ends in TO."
  (should (fons-path-circular-p
           (make-fons-path
            :hops (list (make-fons-hop
                         :from "alice" :to "bob")
                        (make-fons-hop
                         :from "bob" :to "alice")))))
  (should (fons-path-circular-p
           (make-fons-path
            :hops (list (make-fons-hop
                         :from "alice" :to "bob")
                        (make-fons-hop
                         :from "bob" :to "carol")
                        (make-fons-hop
                         :from "carol" :to "bob")))))
  (should-not (fons-path-circular-p
               ;; Ignore circular paths-from-alice before the last hop.
               (make-fons-path
                :hops (list (make-fons-hop
                             :from "alice" :to "bob")
                            (make-fons-hop
                             :from "bob" :to "alice")
                            (make-fons-hop
                             :from "alice" :to "carol"))))))

(ert-deftest fons-path-alice-to-eve-relation-score ()
  "Relation score."
  (fons-test ()
    (let* ((from "alice") (to "eve")
           (paths-about (fons-paths from "tofu"))
           (relation (fons-relation to paths-about)))
      (should (= 2 (length (fons-relation-paths relation))))
      (should (= 0.9120000000000001 (fons-relation-score relation))))))

(ert-deftest fons-relations ()
  "Relations from Alice."
  (fons-test ()
    (let ((relations (fons-relations "alice" "tofu")))
      (should (= 3 (hash-table-count relations)))
      (should (= 0.6400000000000001
                 (fons-relation-score (gethash "david" relations))))
      (should (= 0.5120000000000001
                 (fons-relation-score (gethash "eve" relations))))
      (should (= 0.8
                 (fons-relation-score (gethash "carol" relations)))))
    (let ((relations (fons-relations "alice" "tofu" :threshold 0)))
      (should (= 4 (hash-table-count relations)))
      (should (= 0.25
                 (fons-relation-score (gethash "bob" relations))))
      (should (= 0.6400000000000001
                 (fons-relation-score (gethash "david" relations))))
      (should (= 0.5120000000000001
                 (fons-relation-score (gethash "eve" relations))))
      (should (= 0.8
                 (fons-relation-score (gethash "carol" relations)))))
    (let ((relations (fons-relations "alice" "tofu" :threshold 0.6)))
      (should (= 2 (hash-table-count relations)))
      (should (= 0.6400000000000001
                 (fons-relation-score (gethash "david" relations))))
      (should (= 0.8
                 (fons-relation-score (gethash "carol" relations)))))
    (let ((relations (fons-relations "alice" "tofu" :max-hops 2)))
      (should (= 2 (hash-table-count relations)))
      (should (= 0.6400000000000001
                 (fons-relation-score (gethash "david" relations))))
      (should (= 0.8
                 (fons-relation-score (gethash "carol" relations)))))
    (let ((relations (fons-relations "alice" "tofu" :max-hops 1)))
      (should (= 1 (hash-table-count relations)))
      (should (= 0.8
                 (fons-relation-score (gethash "carol" relations)))))
    (let ((relations (fons-relations "alice" "tofu" :blocked '("carol"))))
      (should (= 0 (hash-table-count relations))))
    (let ((relations (fons-relations "alice" "tofu" :blocked '("david"))))
      (should (= 1 (hash-table-count relations)))
      (should-not (gethash "david" relations))
      ;; A -> C -> E has path score 0.4 (below default 0.5 threshold)
      (should-not (gethash "eve" relations)))
    (let ((relations
           (fons-relations "alice" "tofu" :blocked '("david") :threshold 0.4)))
      (should (= 2 (hash-table-count relations)))
      (should-not (gethash "david" relations)))
    (let ((relations (fons-relations "alice" "tofu" :blocked '("eve"))))
      (should (= 2 (hash-table-count relations)))
      (should-not (gethash "eve" relations)))))

(ert-deftest fons-relations-shorter-path-lower-score ()
  "Reducing max-hops decreases relation score if shorter path has lower score."
  (fons-test ((lambda ()
                (fons-add-hop "alice" "bob" 0.9 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "bob" "carol" 0.9 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "carol" "david" 0.9 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "alice" "eve" 0.8 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "eve" "david" 0.8 "tofu" test-hyperdrive-fons-hops)))
    (let ((relations (fons-relations "alice" "tofu" :max-hops 3)))
      ;; Relation to david includes A -> B -> C -> D and also A -> E -> D.
      (should (= 0.7290000000000001
                 (fons-relation-score (gethash "david" relations)))))
    (let ((relations (fons-relations "alice" "tofu" :max-hops 2)))
      ;; With MAX-HOPS 2, relation to david now only includes A -> E -> D.
      (should (= 0.6400000000000001
                 (fons-relation-score (gethash "david" relations)))))))

(ert-deftest fons-relations-many-hops ()
  "Relations from Alice with max-hops set to 1."
  (fons-test ((lambda ()
                (dotimes (from 10)
                  (dotimes (to 10)
                    (unless (= from to)
                      (fons-add-hop from to 0.5 "tofu" test-hyperdrive-fons-hops))))))
    (let ((relations (fons-relations 0 "tofu" :max-hops 1)))
      (should (= 9 (hash-table-count relations))))
    (let ((relations (fons-relations 0 "tofu" :max-hops 2)))
      ;; FIXME: Yay!  A bug!
      (should (= 9 (hash-table-count relations))))))

;; (ert-deftest fons-relation-view ()
;;   ""
;;   (fons-test ((lambda ()
;;                 (funcall test-hyperdrive-fons-default-hops-fn)
;;                 (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
;;                 (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
;;                 (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops)))
;;     (let* ((from "alice") (to "eve")
;;            (paths-about (fons-paths from "tofu"))
;;            (relation (fons-relation to paths-about)))
;;       (hyperdrive-fons-view relation  :layout "dot"))))

;; (ert-deftest fons-path-view ()
;;   ""
;;   (fons-test ((lambda ()
;;                 (funcall test-hyperdrive-fons-default-hops-fn)
;;                 (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
;;                 (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
;;                 (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops)))
;;     (let* ((froms (delete-dups
;;                    (mapcar (lambda (it)
;;                              (fons-hop-from (cadar it)))
;;                            (map-values test-hyperdrive-fons-hops))))
;;            (paths-by-from (delete-dups
;;                            (mapcar (lambda (from)
;;                                      (cons from (fons-paths from "tofu")))
;;                                    froms))))
;;       (hyperdrive-fons-view (delete-dups (flatten-list (delete-dups (map-values paths-by-from)))) :layout "dot"))))

;; (ert-deftest fons-path-view* ()
;;   ""
;;   (fons-test ((lambda ()
;;                 (funcall test-hyperdrive-fons-default-hops-fn)
;;                 (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
;;                 (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
;;                 (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops)))
;;     (hyperdrive-fons-view (delete-dups
;;                            (flatten-list
;;                             (mapcar #'fons-path-hops
;;                                     (fons-paths "alice" "tofu"))))
;;                           :layout "dot")))

(ert-deftest fons-path-view ()
  "Not a test; used to experiment with graphviz."
  (skip-unless nil)
  (fons-test ((lambda ()
                (funcall test-hyperdrive-fons-default-hops-fn)
                (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "eve" "david" 0.8 "tofu" test-hyperdrive-fons-hops)))
             (let* ((paths-from-alice (fons-paths "alice" "tofu"))
                    (tos (delete-dups
                          (flatten-list
                           (mapcar #'fons-path-tos paths-from-alice))))
                    (relations (mapcar (lambda (to)
                                         (fons-relation to paths-from-alice))
                                       tos)))
               (hyperdrive-fons-view relations "alice" :layout "dot"
                                     ;; :debug t
                                     ))))

(ert-deftest fons-relation-score-threshold ()
  ""
  (fons-test (
              (lambda ()
                (fons-add-hop "alice" "bob" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "alice" "eve" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "eve" "carol" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "bob" "carol" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "carol" "bob" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "carol" "doug" 0.5 "tofu" test-hyperdrive-fons-hops)
                ;; (fons-add-hop "alice" "bob" 0.5 "tofu" test-hyperdrive-fons-hops)
                ;; (fons-add-hop "alice" "carol" 0.5 "tofu" test-hyperdrive-fons-hops)
                ;; (fons-add-hop "bob" "carol" 0.5 "tofu" test-hyperdrive-fons-hops)
                ;; (fons-add-hop "carol" "doug" 0.5 "tofu" test-hyperdrive-fons-hops)
                )
              )
    (let* ((paths-from-alice (fons-paths "alice" "tofu" :threshold 0))
           ;; (froms (mapcar (lambda (path)
           ;;                  (fons-hop-from (car (fons-path-hops path))))
           ;;                paths-from-alice))
           (tos (delete-dups
                 (flatten-list
                  (mapcar #'fons-path-tos paths-from-alice))))
           (relations (mapcar (lambda (to)
                                (fons-relation to paths-from-alice))
                              tos))
           )
      (hyperdrive-fons-view relations "alice" :layout "dot"
                            ;; :debug t
                            )))

  )

(ert-deftest fons-relations-view ()
  "Not a test.  Opens fons-view buffer."
  (skip-unless nil)
  (fons-test ((lambda ()
                (fons-add-hop "alice" "bob" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "alice" "eve" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "eve" "carol" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "bob" "carol" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "carol" "bob" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "carol" "doug" 0.5 "tofu" test-hyperdrive-fons-hops)
                ))
    (let* ((relations (fons-relations "alice" "tofu" :threshold 0)))
      (hyperdrive-fons-view relations "alice" :layout "dot"
                            ;; :debug t
                            ))))

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hf/"  . "hyperdrive-fons-")
;;   ("h/"   . "hyperdrive-"))
;; End:

;;; test-hyperdrive-fons.el ends here
