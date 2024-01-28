;;; hyperdrive-fons.el ---      -*- lexical-binding: t; -*-


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

;;;; Types

(cl-defstruct fons-hop
  from to score)

(cl-defstruct fons-path hops score)

(cl-defstruct fons-relation
  from to paths score)

;;;; Variables

(defvar fons-score-threshold 0.5
  "Paths that score below this are omitted.")

;;;; Functions

(defun fons-add-hop (from to score topic table)
  (let ((hop (make-fons-hop :from from :to to :score score)))
    (push hop (map-elt (map-elt table
                                (fons-hop-from hop))
                       topic))))

(defun fons-hops (_from)
  "Return hops from user FROM."
  (error "Not yet implemented (bound in tests)"))

(cl-defun fons-paths
    (from topic &key (max-hops 3) (threshold fons-score-threshold))
  "Return paths from FROM up to MAX-HOPS in HOPS about TOPIC."
  (cl-labels ((extend-path (a b)
                "Return a copy of path A extended by B and rescored."
                (setf a (copy-sequence a))
                (setf (fons-path-hops b) (append (fons-path-hops a)
                                                 (fons-path-hops b)))
                (setf (fons-path-score b) (fons-score b))
                b))
    (let* ((hops (map-elt (fons-hops from) topic))
           (paths (cl-loop for hop in hops
                           collect (make-fons-path
                                    :score (fons-hop-score hop)
                                    :hops (list hop)))))
      ;; NOTE: At this point, `paths' only has one-hop paths.
      (unless (zerop (cl-decf max-hops))
        ;; Add hops up to the limit.
        (dolist (path paths)
          (let* ((last-hop (car (last (fons-path-hops path))))
                 (last-to (fons-hop-to last-hop))
                 (new-paths (fons-paths
                             last-to topic :max-hops 1 :threshold threshold)))
            (dolist (new-path new-paths)
              (let ((extended-path (extend-path path new-path)))
                (push extended-path paths)
                (unless (< (fons-path-score extended-path) threshold)
                  (let* ((extended-last-hop
                          (car (last (fons-path-hops extended-path))))
                         (extended-last-to (fons-hop-to extended-last-hop))
                         (extended-paths
                          (mapcar (lambda (path)
                                    (extend-path extended-path path))
                                  (fons-paths
                                   extended-last-to topic
                                   :max-hops max-hops :threshold threshold))))
                    (cl-callf2 append extended-paths paths))))))))
      paths)))

(defun fons-score (path)
  "Return PATH's score."
  (let ((hop-number 0))
    (cl-reduce
     (lambda (acc hop)
       (* acc
          (expt (fons-hop-score hop)
                ;; TODO: Consider using an option to soften the decay by length.
                (cl-incf hop-number))))
     (fons-path-hops path)
     :initial-value 1)))

;; (defun fons-path-from-p (from path)
;;   "Return non-nil if PATH is from FROM.")

(defun fons-path-to-p (to path)
  "Return non-nil if PATH is to TO."
  (equal to (fons-hop-to (car (last (fons-path-hops path))))))

;; (cl-defun fons-relation (to paths &key (score-fn #'fons-score-patsh))
;;   "Return relation aggregating PATHS to TO.
;; PATHS should be a list of paths from a single source to TO."
;;   ;; TODO: Consider asserting that all PATHS are from the same source.
;;   (let* ((relation (make-fons-relation :from from :to to)))
;;     (setf (fons-relation-score relation)
;;           ;; compute score from paths
;;           )
;;     relation))

(defun fons-score-relation (relation)
  "Return RELATION's score having aggregated its paths."
  (cl-reduce #'+ (fons-relation-paths relation)
             :key #'fons-path-score))

;;;; Footer

(provide 'hyperdrive-fons)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("hf/"  . "hyperdrive-fons-")
;;   ("h/"   . "hyperdrive-"))
;; End:
