;;; test-hyperdrive-fons.el ---      -*- lexical-binding: t; -*-

(require 'ert)

(require 'map)
(require 'seq)

(require 'hyperdrive-fons)

(defvar test-hyperdrive-fons-hops (make-hash-table :test 'equal))

(defvar test-hyperdrive-fons-default-hops-fn
  (lambda ()
    (fons-add-hop "alice" "bob" 0.25 "tofu" test-hyperdrive-fons-hops)
    (fons-add-hop "alice" "carole" 0.8 "tofu" test-hyperdrive-fons-hops)
    (fons-add-hop "carole" "david" 0.8 "tofu" test-hyperdrive-fons-hops)
    (fons-add-hop "carole" "eve" 0.5 "tofu" test-hyperdrive-fons-hops)
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

(ert-deftest fons-relations ()
  "Not a test; used to experiment."
  (skip-unless nil)
  (fons-test ()
    (let* ((froms (delete-dups
                   (mapcar (lambda (it)
                             (fons-hop-from (cadar it)))
                           (map-values test-hyperdrive-fons-hops))))
           (paths-by-from (mapcar (lambda (from)
                                    (cons from (fons-paths from "tofu")))
                                  froms))
           (relations-by-from
            (mapcar
             (lambda (map)
               (let* ((from (car map))
                      (paths-from-alice (cdr map))
                      (tos (delete-dups
                            (mapcar
                             (lambda (path)
                               (fons-hop-to (car (last (fons-path-hops path)))))
                             paths-from-alice)))
                      (relations
                       (mapcar
                        (lambda (to)
                          (let ((relation (make-fons-relation
                                           :from from :to to
                                           :paths (cl-remove-if-not
                                                   (lambda (path)
                                                     (fons-path-to-p to path))
                                                   paths-from-alice))))
                            (setf (fons-relation-score relation)
                                  (funcall fons-relation-score-fn relation))
                            relation))
                        tos)))
                 relations))
             paths-by-from)))
      (should-not relations-by-from))))

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
                         :from "bob" :to "carole")
                        (make-fons-hop
                         :from "carole" :to "bob")))))
  (should-not (fons-path-circular-p
               ;; Ignore circular paths-from-alice before the last hop.
               (make-fons-path
                :hops (list (make-fons-hop
                             :from "alice" :to "bob")
                            (make-fons-hop
                             :from "bob" :to "alice")
                            (make-fons-hop
                             :from "alice" :to "carole"))))))

(ert-deftest fons-path-alice-to-eve-relation-score ()
  "Relation score."
  (fons-test ()
    (let* ((from "alice") (to "eve")
           (paths-about (fons-paths from "tofu"))
           (relation (fons-relation to paths-about)))
      (should (= 2 (length (fons-relation-paths relation))))
      (should (= 0.9120000000000001 (fons-relation-score relation))))))

(ert-deftest fons-relation-view ()
  ""
  (fons-test ((lambda ()
                (funcall test-hyperdrive-fons-default-hops-fn)
                (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops)))
    (let* ((from "alice") (to "eve")
           (paths-about (fons-paths from "tofu"))
           (relation (fons-relation to paths-about)))
      (hyperdrive-fons-view relation :layout "dot"))))

(ert-deftest fons-path-view ()
  ""
  (fons-test ((lambda ()
                (funcall test-hyperdrive-fons-default-hops-fn)
                (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops)))
    (let* ((froms (delete-dups
                   (mapcar (lambda (it)
                             (fons-hop-from (cadar it)))
                           (map-values test-hyperdrive-fons-hops))))
           (paths-by-from (delete-dups
                           (mapcar (lambda (from)
                                     (cons from (fons-paths from "tofu")))
                                   froms))))
      (hyperdrive-fons-view (delete-dups (flatten-list (delete-dups (map-values paths-by-from)))) :layout "dot"))))

(ert-deftest fons-path-view* ()
  ""
  (fons-test ((lambda ()
                (funcall test-hyperdrive-fons-default-hops-fn)
                (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops)))
    (hyperdrive-fons-view (delete-dups
                           (flatten-list
                            (mapcar #'fons-path-hops
                                    (fons-paths "alice" "tofu"))))
                          :layout "dot")))

(ert-deftest fons-path-view** ()
  ""
  (fons-test ((lambda ()
                (funcall test-hyperdrive-fons-default-hops-fn)
                (fons-add-hop "alice" "frank" 1 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "frank" "georgie" 0.2 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "georgie" "hobart" 0.8 "tofu" test-hyperdrive-fons-hops))
              ;; (lambda ()
              ;;   (fons-add-hop "alice" "bob" 0.5 "tofu" test-hyperdrive-fons-hops)
              ;;   (fons-add-hop "alice" "carole" 0.5 "tofu" test-hyperdrive-fons-hops)
              ;;   (fons-add-hop "bob" "doug" 0.5 "tofu" test-hyperdrive-fons-hops)
              ;;   (fons-add-hop "carole" "doug" 0.5 "tofu" test-hyperdrive-fons-hops)
              ;;   (fons-add-hop "doug" "eve" 0.5 "tofu" test-hyperdrive-fons-hops))
              )
    (let* ((paths-from-alice (fons-paths "alice" "tofu"))
           (hops (delete-dups (flatten-list (mapcar #'fons-path-hops paths-from-alice))))
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
      (hyperdrive-fons-view hops :layout "dot" :relations relations
                            
                            ;; :debug t
                            ))))

(ert-deftest fons-relation-score-threshold ()
  ""
  (fons-test (
              (lambda ()
                (fons-add-hop "alice" "bob" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "alice" "carole" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "bob" "carole" 0.5 "tofu" test-hyperdrive-fons-hops)
                (fons-add-hop "carole" "doug" 0.5 "tofu" test-hyperdrive-fons-hops)
                )
              )
    (let* ((paths-from-alice (fons-paths "alice" "tofu" :threshold 0))
           (hops (delete-dups (flatten-list (mapcar #'fons-path-hops paths-from-alice))))
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
      (hyperdrive-fons-view hops :layout "dot" :relations relations
                            
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
