;; Prototyping trust-computing library.  Working name: sophia.el.

(defvar sophia-topics (make-hash-table :test #'equal))

(defun sophia-add-relation (topic source dest weight topics)
  (setf (plist-get (map-elt (map-elt (map-elt topics topic) source) dest) :weight) weight))

(sophia-add-relation "tofu" "alice" "bob" 0.25 sophia-topics)
(sophia-add-relation "tofu" "alice" "carole" 0.8 sophia-topics)
(sophia-add-relation "tofu" "carole" "david" 0.8 sophia-topics)
(sophia-add-relation "tofu" "carole" "eve" 0.5 sophia-topics)

(sophia-add-relation "tofu" "david" "eve" 0.8 sophia-topics)

;; sophia-topics

;; (defun sophia-decay (a b &optional _depth)
;;   "Return B's weight decayed by A's weight.
;; A and B are alists where the CAR is the name and the CDR is the
;; weight."
;;   (pcase-let* ((`(,_a . ,a-weight) a)
;;                (`(,_b . ,b-weight) b))
;;     (* a b)))

;; (defalias 'sophia-decay
;;   (pcase-lambda (source (and dest `(,_ . ,(map (:weight dest-weight)))) _hops)
;;     (setf (plist-get (cdr dest) :weight) (* a-weight dest-weight))
;;     dest))

(cl-defun sophia-dsts>=
    (source min relations
            &key (limit 5) (hops 1)
            (modifier-fns (list (lambda (a b hops)
                                  (setf (plist-get (cdr b) :hops) hops)
                                  b)))
            (decay-fn #'sophia-decay))
  "Return list of destinations trusted by SOURCE by at least MIN."
  (let* ((decay-fn (pcase-lambda (a (and dest `(,_ . ,(map (:weight dest-weight)))) _hops)
                     (setf (plist-get (cdr dest) :weight) (* a-weight dest-weight))
                     dest))
         (dsts (map-elt relations source)))
    (when modifier-fns
      (dolist (fn modifier-fns)
        (setf dsts (mapcar (lambda (b)
                             (funcall fn source b hops))
                           dsts))))
    (setf dsts (map-filter (pcase-lambda (dst (map :weight))
                             (>= weight min))
                           dsts))
    (append dsts
            (unless (zerop limit)
              (mapcar (pcase-lambda (`(,src . ,(map (:weight a-weight))))
                        (sophia-dsts>= src min relations :limit (1- limit) :hops (1+ hops)
                                       :modifier-fns (cons decay-fn modifier-fns)))
                      dsts)))))

(sophia-dsts>= "alice" 0.2 (map-elt sophia-topics "tofu"))


