;;; hyperdrive-fons.el ---      -*- lexical-binding: t; -*-


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

;;;; Types

(cl-defstruct fons-hop from to score)
(cl-defstruct fons-path hops score)
(cl-defstruct fons-relation from to paths score)

;;;; Variables

(defvar fons-path-score-threshold 0.5
  "Paths that score below this are omitted.")

(defgroup fons nil
  "FIXME: Docstring."
  :group 'applications)

(defcustom fons-path-score-fn #'fons-path-score-default
  "Path scoring function.
Takes one argument, a `fons-path' and returns a number from 0 to
1."
  :type 'function)

(defcustom fons-path-score-decay-coefficient 1
  "FIXME:"
  :type 'number)

(defcustom fons-relation-score-fn #'fons-relation-score-default
  "FIXME: Docstring."
  :type 'function)

;;;; Functions

(defun fons-hops (_from)
  "Return hops from user FROM."
  (error "Not yet implemented (bound in tests)"))

(cl-defun fons-relations
    (root topic &key blocked (max-hops 3) (threshold fons-path-score-threshold))
  "Return a table of `fons-relation' structs from ROOT about TOPIC.
Recurses up to MAX-HOPS times, returning only relations whose
scores are above THRESHOLD which are not in BLOCKED."
  (unless (and (integerp max-hops) (cl-plusp max-hops))
    (error "MAX-HOPS must be an positive integer"))
  (when (member root blocked)
    (error "BLOCKED must not contain ROOT"))
  (let ((relations (make-hash-table :test 'equal)))
    (cl-labels ((add-relations-from (from &optional paths-to-from)
                  (dolist (hop (map-elt (fons-hops from) topic))
                    (when-let ((to-relation
                                (and (not (equal root (fons-hop-to hop)))
                                     (not (member (fons-hop-to hop) blocked))
                                     (ensure-relation (fons-hop-to hop))))
                               (paths-to-to
                                (if paths-to-from
                                    ;; `extended-paths' may return nil if the only
                                    ;; paths to TO are circular.
                                    (extended-paths paths-to-from hop)
                                  ;; On the 1st hop, paths-to-from is nil.
                                  (list (make-fons-path :hops (list hop))))))
                      (score-paths paths-to-to)
                      (update-relation to-relation paths-to-to)
                      (when (and (above-threshold-p to-relation)
                                 (within-max-hops-p to-relation))
                        (add-relations-from (fons-relation-to to-relation)
                                            paths-to-to)))))
                (update-relation (relation paths)
                  (setf (fons-relation-paths relation)
                        (append paths (fons-relation-paths relation)))
                  (setf (fons-relation-score relation)
                        (funcall fons-relation-score-fn relation)))
                (extended-paths (paths hop)
                  "Return list of PATHS extended by HOP without circular hops."
                  (remq nil
                        (mapcar
                         (lambda (path)
                           (unless (circular-p path hop)
                             (make-fons-path
                              :hops (append (fons-path-hops path) (list hop)))))
                         paths)))
                (circular-p (path last-hop)
                  "Return non-nil when HOP circles back to any hop in PATH."
                  (cl-some (lambda (hop)
                             (equal (fons-hop-to last-hop) (fons-hop-from hop)))
                           (fons-path-hops path)))
                (score-paths (paths)
                  (dolist (path paths)
                    (setf (fons-path-score path)
                          (funcall fons-path-score-fn path))))
                (above-threshold-p (relation)
                  (>= (fons-relation-score relation) threshold))
                (within-max-hops-p (relation)
                  (cl-some (lambda (path)
                             (length< (fons-path-hops path) max-hops))
                           (fons-relation-paths relation)))
                (ensure-relation (to)
                  "Add relation from ROOT to TO if none exists, then return it."
                  (or (gethash to relations)
                      (setf (gethash to relations)
                            (make-fons-relation :from root :to to)))))
      (add-relations-from root)
      (maphash (lambda (to relation)
                 (unless (above-threshold-p relation)
                   (remhash to relations)))
               relations)
      relations)))

(defun fons-path-score-default (path)
  "Return PATH's score."
  (let ((decay-power 0))
    (cl-reduce
     (lambda (acc hop)
       (* acc
          (fons-hop-score hop)
          (prog1 (expt fons-path-score-decay-coefficient decay-power)
            (cl-incf decay-power))))
     (fons-path-hops path)
     :initial-value 1)))

(defun fons-path-from-p (from path)
  "Return non-nil if PATH is from FROM."
  (equal from (fons-hop-from (car (fons-path-hops path)))))

(defun fons-relation-score-default (relation)
  "Return RELATION's score based on the scores of its paths.
If RELATION contains a single-hop path, return that path's score.
Otherwise, return the highest path score among all paths.
The returned score does not exceed 1."
  (let* ((paths (fons-relation-paths relation))
         ;; TODO: Instead of `cl-find' then `cl-reduce', just iterate once.
         (direct-path (cl-find (lambda (path)
                                 (length= 1 (fons-path-hops path)))
                               paths)))
    (if direct-path
        (fons-path-score direct-path)
      (cl-reduce #'max (fons-relation-paths relation)
                 :key #'fons-path-score))))

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

;;; hyperdrive-fons.el ends here
