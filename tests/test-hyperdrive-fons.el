;;; test-hyperdrive-fons.el ---      -*- lexical-binding: t; -*-

(require 'ert)

(require 'map)
(require 'seq)

(require 'hyperdrive-fons)
(require 'hyperdrive-fons-view)

(defvar test-hyperdrive-fons-hops (make-hash-table :test 'equal))

(cl-defun fons-test-add-hop (from to table &optional topic)
  (push (make-fons-hop :from from :to to)
        (map-elt (map-elt table from) (or topic fons-default-topic))))

(defvar test-hyperdrive-fons-default-hops-fn
  (lambda ()
    (fons-test-add-hop "alice" "bob" test-hyperdrive-fons-hops)
    (fons-test-add-hop "alice" "carol" test-hyperdrive-fons-hops)
    (fons-test-add-hop "carol" "david" test-hyperdrive-fons-hops)
    (fons-test-add-hop "carol" "eve" test-hyperdrive-fons-hops)
    (fons-test-add-hop "david" "eve" test-hyperdrive-fons-hops)
    (fons-test-add-hop "eve" "mallory" test-hyperdrive-fons-hops)))

(defvar test-hyperdrive-fons-default-blockers-fn
  (lambda ()
    (fons-test-add-hop "alice" "bob" test-hyperdrive-fons-hops fons-blocker-topic)
    (fons-test-add-hop "bob" "carol" test-hyperdrive-fons-hops fons-blocker-topic)))

(defvar test-hyperdrive-fons-blocked (make-hash-table :test 'equal)
  "Keyed by BLOCKER, value is a list of BLOCKED.")

(defvar test-hyperdrive-fons-default-blocked-fn
  (lambda ()
    (puthash "bob" '("mallory" "darth") test-hyperdrive-fons-blocked)
    (puthash "carol" '("mallory") test-hyperdrive-fons-blocked)))

(cl-defmacro fons-test ((&optional hops-fn blockers-fn blocked-fn) &rest body)
  (declare (indent defun) (debug (([&optional lambda-expr]) def-body)))
  `(progn
     (clrhash test-hyperdrive-fons-hops)
     (funcall (or ,hops-fn
                  test-hyperdrive-fons-default-hops-fn))
     (funcall (or ,blockers-fn
                  test-hyperdrive-fons-default-blockers-fn))
     (funcall (or ,blocked-fn
                  test-hyperdrive-fons-default-blocked-fn))
     (cl-letf* (
                (test-fons-hops-fn
                 (lambda (peer-name topic)
                   (map-elt (gethash peer-name test-hyperdrive-fons-hops) topic)))
                ((symbol-function 'fons-direct-blocks)
                 (lambda (blocker)
                   (gethash blocker test-hyperdrive-fons-blocked))))
       ,@body)))

;;;; Tests

(ert-deftest fons-blockers-blocked-sources ()
  "Roundtrip test with stubs.
1. Get BLOCKERS from ROOT.
2. Get BLOCKED from BLOCKERS.
3. Get SOURCES from ROOT while excluding BLOCKED."
  (fons-test ()
    (let* ((blockers (fons-relations "alice" :topic fons-blocker-topic
                                     :hops-fn test-fons-hops-fn))
           (blocked (fons-blocked blockers))
           (sources (fons-relations "alice" :blocked blocked
                                    :hops-fn test-fons-hops-fn)))
      (should (= 2 (hash-table-count blockers)))
      ;; Note that Bob is a BLOCKER but not a SOURCE since his BLOCKER score is
      ;; above the threshold while his `fons-default-topic' score is not.
      (should (gethash "bob" blockers))
      (should (gethash "carol" blockers))

      (should (= 2 (hash-table-count blocked)))
      (should (gethash "mallory" blocked))
      ;; Note that Darth is BLOCKED but not among BLOCKED-SOURCES since nobody
      ;; added him as a SOURCE anyway.
      (should (gethash "darth" blocked))

      (should (= 4 (hash-table-count sources)))
      (should-not (fons-relation-blocked-p (gethash "carol" sources)))
      (should-not (fons-relation-blocked-p (gethash "david" sources)))
      (should-not (fons-relation-blocked-p (gethash "eve" sources)))
      (should (fons-relation-blocked-p (gethash "mallory" sources))))))

(ert-deftest fons-relations ()
  "Relations from Alice."
  (fons-test ()
    (let ((relations (fons-relations "alice" :hops-fn test-fons-hops-fn)))
      (should (= 5 (hash-table-count relations)))
      (should (gethash "david" relations))
      (should (gethash "eve" relations))
      (should (gethash "carol" relations))
      (should (gethash "eve" relations))
      (should (gethash "mallory" relations)))
    (let ((relations (fons-relations "alice" :threshold 0
                                     :hops-fn test-fons-hops-fn)))
      (should (= 5 (hash-table-count relations)))
      (should (gethash "bob" relations))
      (should (gethash "david" relations))
      (should (gethash "eve" relations))
      (should (gethash "carol" relations)))
    (let ((relations (fons-relations "alice" :hops-fn test-fons-hops-fn)))
      (should (= 2 (hash-table-count relations)))
      (should (gethash "david" relations))
      (should (gethash "carol" relations)))
    (let ((relations (fons-relations "alice" :max-hops 2
                                     :hops-fn test-fons-hops-fn)))
      (should (= 2 (hash-table-count relations)))
      (should (gethash "david" relations))
      (should (gethash "carol" relations)))
    (let ((relations (fons-relations "alice" :max-hops 1
                                     :hops-fn test-fons-hops-fn)))
      (should (= 1 (hash-table-count relations)))
      (should (gethash "carol" relations)))
    (let* ((blocked (make-hash-table :test 'equal))
           (_ (puthash "carol" "foo" blocked))
           (relations (fons-relations "alice" :blocked blocked
                                      :hops-fn test-fons-hops-fn)))
      (should (= 1 (hash-table-count relations)))
      (should (fons-relation-blocked-p (gethash "carol" relations))))
    (let* ((blocked (make-hash-table :test 'equal))
           (_ (puthash "david" "foo" blocked))
           (relations (fons-relations "alice" :blocked blocked
                                      :hops-fn test-fons-hops-fn)))
      (should (= 2 (hash-table-count relations)))
      ;; A -> C -> E has path score 0.4 (below default 0.5 threshold).
      (should-not (gethash "eve" relations))
      (should (fons-relation-blocked-p (gethash "david" relations))))
    (let* ((blocked (make-hash-table :test 'equal))
           (_ (puthash "david" "foo" blocked))
           (relations (fons-relations "alice" :blocked blocked
                                      :hops-fn test-fons-hops-fn)))
      (should (= 3 (hash-table-count relations)))
      (should (fons-relation-blocked-p (gethash "david" relations))))
    (let* ((blocked (make-hash-table :test 'equal))
           (_ (puthash "eve" "foo" blocked))
           (relations (fons-relations "alice" :blocked blocked
                                      :hops-fn test-fons-hops-fn)))
      (should (= 3 (hash-table-count relations)))
      (should (fons-relation-blocked-p (gethash "eve" relations))))))

(ert-deftest fons-blocked ()
  "Blocked peers."
  (fons-test ()
    (ignore test-fons-hops-fn)
    (let ((blockers (make-hash-table :test #'equal)))
      (puthash "bob" 'blocker-relations-to-bob blockers)
      (puthash "carol" 'blocker-relations-to-carol blockers)
      (should (= 2 (hash-table-count (fons-blocked blockers)))))))

(ert-deftest fons-relations-many-hops ()
  "Relations from Alice with max-hops set to 1."
  (fons-test ((lambda ()
                (dotimes (from 6)
                  (dotimes (to 6)
                    (unless (= from to)
                      (fons-test-add-hop from to test-hyperdrive-fons-hops))))))
    (let ((relations (fons-relations 0 :max-hops 5
                                     :hops-fn test-fons-hops-fn)))
      (should (= 5 (hash-table-count relations)))
      ;; (hyperdrive-fons-view relations 0)
      )))

;;;;; GUI

(ert-deftest fons-relations-view ()
  "Not a test.  Opens fons-view buffer."
  (skip-unless nil)
  (fons-test ((lambda ()
                (fons-test-add-hop "alice" "bob" test-hyperdrive-fons-hops)
                (fons-test-add-hop "alice" "eve" test-hyperdrive-fons-hops)
                (fons-test-add-hop "eve" "carol" test-hyperdrive-fons-hops)
                (fons-test-add-hop "bob" "carol" test-hyperdrive-fons-hops)
                (fons-test-add-hop "carol" "bob" test-hyperdrive-fons-hops)
                (fons-test-add-hop "carol" "doug" test-hyperdrive-fons-hops)
                ))
    (let* ((relations (fons-relations "alice" :hops-fn test-fons-hops-fn)))
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
