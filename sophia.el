;; Prototyping trust-computing library.  Working name: sophia.el.

(defvar sophia-topics (make-hash-table :test #'equal))

(defun sophia-add-relation (topic source dest weight topics)
  (setf (plist-get (map-elt (map-elt (map-elt topics topic) source) dest) :weight) weight))

(defmacro sophia-test (&rest body)
  `(progn
     (clrhash sophia-topics)
     (sophia-add-relation "tofu" "alice" "bob" 0.25 sophia-topics)
     (sophia-add-relation "tofu" "alice" "carole" 0.8 sophia-topics)
     (sophia-add-relation "tofu" "carole" "david" 0.8 sophia-topics)
     (sophia-add-relation "tofu" "carole" "eve" 0.5 sophia-topics)
     (sophia-add-relation "tofu" "david" "eve" 0.8 sophia-topics)
     ,@body))

;; sophia-topics

;; (defun sophia-decay (a b &optional _depth)
;;   "Return B's weight decayed by A's weight.
;; A and B are alists where the CAR is the name and the CDR is the
;; weight."
;;   (pcase-let* ((`(,_a . ,a-weight) a)
;;                (`(,_b . ,b-weight) b))
;;     (* a b)))

;; (cl-defun sophia-dsts>=
;;     (source min relations
;;             &key (limit 5) (hops 1)
;;             (modifier-fns (list (lambda (a b hops)
;;                                   (setf (plist-get (cdr b) :hops) hops)
;;                                   b)
;;                                 #'sophia-decay)))
;;   "Return list of destinations trusted by SOURCE by at least MIN."
;;   (let* (;; (decay-fn (pcase-lambda (a (and dest `(,_ . ,(map (:weight dest-weight)))) _hops)
;;          ;;             (setf (plist-get (cdr dest) :weight) (* a-weight dest-weight))
;;          ;;             dest))
;;          (dsts (map-elt relations source)))
;;     (when modifier-fns
;;       (let ((source (cl-typecase source
;;                       (string (list source :weight 1.0))
;;                       (otherwise source))))
;;         (dolist (fn modifier-fns)
;;           (setf dsts (mapcar (lambda (b)
;;                                (funcall fn source b hops))
;;                              dsts)))))
;;     (setf dsts (map-filter (pcase-lambda (dst (map :weight))
;;                              (>= weight min))
;;                            dsts))
;;     (append dsts
;;             (unless (zerop limit)
;;               (mapcar (pcase-lambda (`(,src . ,(map (:weight a-weight))))
;;                         (sophia-dsts>= src min relations :limit (1- limit) :hops (1+ hops)
;;                                        :modifier-fns modifier-fns))
;;                       dsts)))))

(defalias 'sophia-decay
  (pcase-lambda ((and source `(,_ . ,(map (:weight src-weight))))
                 (and dest `(,_ . ,(map (:weight dest-weight))))
                 hops)
    (unless (zerop hops)
      (setf (plist-get (cdr dest) :weight) (* src-weight dest-weight)))
    dest))

(cl-defun sophia-filter
    (source value relations 
            &key (limit 5) (hops 1) (predicate #'>=)
            (modifier-fns (list (lambda (a b hops)
                                  (setf (plist-get (cdr b) :hops) hops)
                                  b)
                                #'sophia-decay)))
  "Return list of destinations trusted by SOURCE by at least MIN."
  (let* ((destinations (map-elt relations source)))
    (when modifier-fns
      (let ((source (cl-typecase source
                      (string (list source :weight 1.0))
                      (otherwise source))))
        (dolist (fn modifier-fns)
          (setf destinations (mapcar (lambda (b)
                                       (funcall fn source b hops))
                                     destinations)))))
    (setf destinations (map-filter (pcase-lambda (dst (map :weight))
                                     (funcall predicate weight value))
                                   destinations))
    (remq nil
          (append destinations
                  (unless (zerop limit)
                    (mapcar (pcase-lambda (src)
                              (sophia-filter src value relations :limit (1- limit) :hops (1+ hops)
                                             :modifier-fns modifier-fns))
                            destinations))))))

(sophia-test
 (sophia-filter "alice" 0.2 (map-elt sophia-topics "tofu")))

;; (("carole" :hops 1 :weight 0.8)
;;  ("bob" :hops 1 :weight 0.25)
;;  (("eve" :hops 2 :weight 0.4)
;;   ("david" :hops 2 :weight 0.6400000000000001)
;;   (("eve" :hops 3 :weight 0.5120000000000001))))

