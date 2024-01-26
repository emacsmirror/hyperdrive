;;; hyperdrive-fons.el ---      -*- lexical-binding: t; -*-


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

;;;; Types

(cl-defstruct fons-hop
  from to score)

(cl-defstruct fons-path hops score)

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
  (cl-reduce #'* (fons-path-hops path) :key #'fons-hop-score))

;; (cl-defun fons-filter-to (to paths)
;;   "Return PATHS that end at TO."
;;   (cl-remove-if-not
;;    (lambda (path)
;;      (equal to (fons-hop-to (car (last (fons-path-hops path))))))
;;    paths))

;; (fons-filter-to "eve" paths)

;; (defcustom fons-aggregate-fn #'fons-aggregate-score-default
;;   "Path aggregation function."
;;   :type 'function
;;   :group 'hyperdrive)

;; (defun fons-aggregate-score (paths)
;;   "Return aggregate score of PATHS."
;;   (funcall fons-aggregate-fn paths))

;; (defun fons-aggregate-score-default (paths)
;;   "Return the aggregate score for PATHS."
;;   ;; TODO: Consider using a weighted mean based on path length.
;;   (cl-loop for path in paths
;;            maximizing (fons-path-score path)))

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
