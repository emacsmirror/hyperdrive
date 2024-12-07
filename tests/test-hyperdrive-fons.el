;;; hyperdrive-sbb-test.el ---      -*- lexical-binding: t; -*-

(require 'ert)
(require 'map)
(require 'seq)

(require 'hyperdrive-sbb)
(require 'hyperdrive-sbb-view)

(cl-defstruct (h/sbb-test-user (:constructor h/sbb-test-user-create))
  id sources blockers blocked)

(defvar h/sbb-test-sbb-users
  ;; NOTE: This variable's value is a form to be used in the macro.
  '(list (h/sbb-test-user-create :id "alice" :blockers '("bob")
                                 :sources '("bob" "carol"))
         (h/sbb-test-user-create :id "bob" :blocked '("mallory" "darth")
                                 :blockers '("carol"))
         (h/sbb-test-user-create :id "carol" :blocked '("mallory")
                                 :sources '("david" "eve"))
         (h/sbb-test-user-create :id "david" :sources '("eve"))
         (h/sbb-test-user-create :id "eve" :sources '("mallory"))
         (h/sbb-test-user-create :id "mallory")))

;; (cl-defmacro h/sbb-test ((&key (users h/sbb-test-sbb-users)) &rest body)
;;   "Within BODY, `users' is bound to the value of the USERS form."
;;   (declare (indent defun) (debug (([&optional lambda-expr]) def-body)))
;;   `(let ((users ,users))
;;      (cl-letf*
;;          ((h/sbb-test-blockers-fn
;;            (lambda (from)
;;              (mapcar (lambda (to)
;;                        (h/sbb-hop-create :from from :to to))
;;                      (h/sbb-test-user-blockers
;;                       (cl-find from users :key #'h/sbb-test-user-id :test #'equal)))))
;;           (h/sbb-test-hops-fn
;;            (lambda (from)
;;              (mapcar (lambda (to)
;;                        (h/sbb-hop-create :from from :to to))
;;                      (sbb-user-sources
;;                       (cl-find from users :key #'h/sbb-test-user-id :test #'equal)))))
;;           (h/sbb-test-blocked-fn
;;            (lambda (blocker)
;;              (gethash blocker test-hyperdrive-sbb-blocked))))
;;        ,@body)))

;;;; Tests

;; (ert-deftest sbb-blockers-blocked-sources ()
;;   "Roundtrip test with stubs.
;; 1. Get BLOCKERS from ROOT.
;; 2. Get BLOCKED from BLOCKERS.
;; 3. Get SOURCES from ROOT while excluding BLOCKED."
;;   (h/sbb-test ()
;;     (let* ((blockers (sbb-relations "alice" :topic sbb-blocker-topic
;;                                      :hops-fn test-sbb-hops-fn))
;;            (blocked (sbb-blocked blockers))
;;            (sources (sbb-relations "alice" :blocked blocked
;;                                     :hops-fn test-sbb-hops-fn)))
;;       (should (= 2 (hash-table-count blockers)))
;;       ;; Note that Bob is a BLOCKER but not a SOURCE since his BLOCKER score is
;;       ;; above the threshold while his `sbb-default-topic' score is not.
;;       (should (gethash "bob" blockers))
;;       (should (gethash "carol" blockers))

;;       (should (= 2 (hash-table-count blocked)))
;;       (should (gethash "mallory" blocked))
;;       ;; Note that Darth is BLOCKED but not among BLOCKED-SOURCES since nobody
;;       ;; added him as a SOURCE anyway.
;;       (should (gethash "darth" blocked))

;;       (should (= 4 (hash-table-count sources)))
;;       (should-not (sbb-relation-blocked-p (gethash "carol" sources)))
;;       (should-not (sbb-relation-blocked-p (gethash "david" sources)))
;;       (should-not (sbb-relation-blocked-p (gethash "eve" sources)))
;;       (should (sbb-relation-blocked-p (gethash "mallory" sources))))))

;; (ert-deftest sbb-blockers-blocked-sources ()
;;   "Roundtrip test with stubs.
;; 1. Get BLOCKERS from ROOT.
;; 2. Get BLOCKED from BLOCKERS.
;; 3. Get SOURCES from ROOT while excluding BLOCKED."
;;   (h/sbb-test ()
;;     (let* ((topic "foo")
;;            (root (cl-find "alice" users :key #'h/sbb-test-user-id :test #'equal))
;;            (blocker-relations
;;             (sbb-relations root :hops-fn test-sbb-blockers-fn))
;;            ;; FIXME: blocked.
;;            (blocked-relations )
;;            (sources-relations
;;             (sbb-relations root
;;                            (lambda (root)
;;                              (mapcar (lambda (source)
;;                                        (h/sbb-hop-create :from root :to source))
;;                                      (map-elt (sbb-user-sources root) topic)))))
;;            )
;;       (should (= 2 (hash-table-count blockers)))
;;       ;; Note that Bob is a BLOCKER but not a SOURCE since his BLOCKER score is
;;       ;; above the threshold while his `sbb-default-topic' score is not.
;;       (should (gethash "bob" blockers))
;;       (should (gethash "carol" blockers))

;;       (should (= 2 (hash-table-count blocked)))
;;       (should (gethash "mallory" blocked))
;;       ;; Note that Darth is BLOCKED but not among BLOCKED-SOURCES since nobody
;;       ;; added him as a SOURCE anyway.
;;       (should (gethash "darth" blocked))

;;       (should (= 4 (hash-table-count sources)))
;;       (should-not (sbb-relation-blocked-p (gethash "carol" sources)))
;;       (should-not (sbb-relation-blocked-p (gethash "david" sources)))
;;       (should-not (sbb-relation-blocked-p (gethash "eve" sources)))
;;       (should (sbb-relation-blocked-p (gethash "mallory" sources))))))

;; (ert-deftest sbb-relations ()
;;   "Relations from Alice."
;;   (h/sbb-test ()
;;     (let ((relations (sbb-relations "alice" :hops-fn test-sbb-hops-fn)))
;;       (should (= 5 (hash-table-count relations)))
;;       (should (gethash "david" relations))
;;       (should (gethash "eve" relations))
;;       (should (gethash "carol" relations))
;;       (should (gethash "eve" relations))
;;       (should (gethash "mallory" relations)))
;;     (let ((relations (sbb-relations "alice" :threshold 0
;;                                     :hops-fn test-sbb-hops-fn)))
;;       (should (= 5 (hash-table-count relations)))
;;       (should (gethash "bob" relations))
;;       (should (gethash "david" relations))
;;       (should (gethash "eve" relations))
;;       (should (gethash "carol" relations)))
;;     (let ((relations (sbb-relations "alice" :hops-fn test-sbb-hops-fn)))
;;       (should (= 2 (hash-table-count relations)))
;;       (should (gethash "david" relations))
;;       (should (gethash "carol" relations)))
;;     (let ((relations (sbb-relations "alice" :max-hops 2
;;                                     :hops-fn test-sbb-hops-fn)))
;;       (should (= 2 (hash-table-count relations)))
;;       (should (gethash "david" relations))
;;       (should (gethash "carol" relations)))
;;     (let ((relations (sbb-relations "alice" :max-hops 1
;;                                     :hops-fn test-sbb-hops-fn)))
;;       (should (= 1 (hash-table-count relations)))
;;       (should (gethash "carol" relations)))
;;     (let* ((blocked (make-hash-table :test 'equal))
;;            (_ (puthash "carol" "foo" blocked))
;;            (relations (sbb-relations "alice" :blocked blocked
;;                                      :hops-fn test-sbb-hops-fn)))
;;       (should (= 1 (hash-table-count relations)))
;;       (should (sbb-relation-blocked-p (gethash "carol" relations))))
;;     (let* ((blocked (make-hash-table :test 'equal))
;;            (_ (puthash "david" "foo" blocked))
;;            (relations (sbb-relations "alice" :blocked blocked
;;                                      :hops-fn test-sbb-hops-fn)))
;;       (should (= 2 (hash-table-count relations)))
;;       ;; A -> C -> E has path score 0.4 (below default 0.5 threshold).
;;       (should-not (gethash "eve" relations))
;;       (should (sbb-relation-blocked-p (gethash "david" relations))))
;;     (let* ((blocked (make-hash-table :test 'equal))
;;            (_ (puthash "david" "foo" blocked))
;;            (relations (sbb-relations "alice" :blocked blocked
;;                                      :hops-fn test-sbb-hops-fn)))
;;       (should (= 3 (hash-table-count relations)))
;;       (should (sbb-relation-blocked-p (gethash "david" relations))))
;;     (let* ((blocked (make-hash-table :test 'equal))
;;            (_ (puthash "eve" "foo" blocked))
;;            (relations (sbb-relations "alice" :blocked blocked
;;                                      :hops-fn test-sbb-hops-fn)))
;;       (should (= 3 (hash-table-count relations)))
;;       (should (sbb-relation-blocked-p (gethash "eve" relations))))))

;; (ert-deftest sbb-blocked ()
;;   "Blocked peers."
;;   (h/sbb-test ()
;;     (ignore test-sbb-hops-fn)
;;     (let ((blockers (make-hash-table :test #'equal)))
;;       (puthash "bob" 'blocker-relations-to-bob blockers)
;;       (puthash "carol" 'blocker-relations-to-carol blockers)
;;       (should (= 2 (hash-table-count (sbb-blocked blockers)))))))

;; (ert-deftest sbb-relations-many-hops ()
;;   "Relations from Alice with max-hops set to 1."
;;   (h/sbb-test ((lambda ()
;;                  (dotimes (from 6)
;;                    (dotimes (to 6)
;;                      (unless (= from to)
;;                        (sbb-test-add-hop from to test-hyperdrive-sbb-hops))))))
;;               (let ((relations (sbb-relations 0 :max-hops 5
;;                                               :hops-fn test-sbb-hops-fn)))
;;                 (should (= 5 (hash-table-count relations)))
;;                 ;; (hyperdrive-sbb-view relations 0)
;;                 )))

;;;;; GUI

;; (ert-deftest sbb-relations-view ()
;;   "not a test.  opens sbb-view buffer."
;;   (skip-unless nil)
;;   (h/sbb-test
;;     (:users (list (h/sbb-test-user-create :id "alice" :blockers '("bob")
;;                                           :sources '(("foo" . ("bob" "eve"))))
;;                   (h/sbb-test-user-create :id "bob"
;;                                           :sources '(("foo" . ("carol")))
;;                                           ;; todo: implement blockers/blocked.
;;                                           :blocked '("mallory" "darth"))
;;                   (h/sbb-test-user-create :id "carol"
;;                                           :sources '(("foo" . ("bob" "doug" "mallory"))))
;;                   (h/sbb-test-user-create :id "eve"
;;                                           :sources '(("foo" . ("carol"))))
;;                   (h/sbb-test-user-create :id "mallory")))
;;     (let* ((blocked )
;;            (relations (sbb-relations "alice"
;;                                      :hops-fn (apply-partially test-sbb-hops-fn "foo")
;;                                      )))
;;       (hyperdrive-sbb-view relations "alice" :layout "dot"
;;                            ;; :debug t
;;                            ))))

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hf/"  . "hyperdrive-sbb-")
;;   ("h/"   . "hyperdrive-"))
;; End:

;;; test-hyperdrive-sbb.el ends here
