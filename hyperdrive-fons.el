;;; hyperdrive-fons.el ---      -*- lexical-binding: t; -*-


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

;;;; Types

(cl-defstruct fons-hop
  from to score)

;; TODO: Consider adding a `:to' slot to avoid
;; (fons-hop-to (car (last (fons-path-hops path))))
(cl-defstruct fons-path hops score)

(cl-defstruct fons-relation
  from to paths score)

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

(defun fons-add-hop (from to score topic table)
  (let ((hop (make-fons-hop :from from :to to :score score)))
    (push hop (map-elt (map-elt table
                                (fons-hop-from hop))
                       topic))))

(defun fons-hops (_from)
  "Return hops from user FROM."
  (error "Not yet implemented (bound in tests)"))

(cl-defun fons-paths
    (from topic &key (max-hops 3) (threshold fons-path-score-threshold)
          (relations (make-hash-table :test 'equal)))
  "Return paths from FROM up to MAX-HOPS in HOPS about TOPIC."
  (cl-labels ((extend-path (a b)
                "Return a copy of path A extended by B and rescored."
                (setf a (copy-sequence a))
                (setf (fons-path-hops b) (append (fons-path-hops a)
                                                 (fons-path-hops b)))
                (setf (fons-path-score b) (funcall fons-path-score-fn b))
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
                  ;; TODO: Recurse into sources whose relation score ends up
                  ;; higher than each individual path score.
                  ;;
                  ;; `fons-relation-score-default' never returns a value above the
                  ;; highest path score, so using the path score is fine for now.
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

(cl-defun fons-relations
    (from topic &key blocked (max-hops 3) (threshold fons-path-score-threshold))
  "Return a table of `fons-relation' structs from FROM about TOPIC.
Recurses up to MAX-HOPS times, returning only relations whose
scores are above THRESHOLD which are not in BLOCKED."
  (unless (and (integerp max-hops) (cl-plusp max-hops))
    (error "MAX-HOPS must be an positive integer"))
  (when (member from blocked)
    (error "BLOCKED must not contain FROM"))
  (let ((relations (make-hash-table :test 'equal)))
    (cl-labels ((add-relations-from (from &optional paths-to-from)
                  (dolist (hop (map-elt (fons-hops from) topic))
                    (when-let ((to-relation
                                (and (not (member (fons-hop-to hop) blocked))
                                     (ensure-relation (fons-hop-to hop))))
                               (paths-to-to
                                (if paths-to-from
                                    (extend-paths paths-to-from hop)
                                  ;; On the 1st hop, paths is nil.
                                  (list (make-fons-path :hops (list hop))))))
                      (score-paths paths-to-to)
                      (update-relation to-relation paths-to-to)
                      (when (and (above-threshold-p to-relation)
                                 (within-max-hops-p to-relation))
                        (add-relations-from (fons-relation-to to-relation)
                                            paths-to-to)))))
                (update-relation (relation paths)
                  (setf (fons-relation-paths relation)
                        ;; TODO: Consider using `cons' to prepend paths and then use `reverse'.
                        ;; TODO: Order of `append' args?  Efficiency?  Effect on graph view?
                        (append (fons-relation-paths relation) paths))
                  (setf (fons-relation-score relation)
                        (funcall fons-relation-score-fn relation)))
                (extend-paths (paths hop)
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
                  "Add a relation to TO if none exists.  Returns relation."
                  (or (gethash to relations)
                      (setf (gethash to relations)
                            (make-fons-relation :from from :to to)))))
      (add-relations-from from)
      (maphash (lambda (to relation)
                 (unless (above-threshold-p relation)
                   (remhash to relations)))
               relations)
      relations)))

(defun fons-path-tos (path)
  "Return all destinations in PATH."
  (mapcar (lambda (hop)
            (fons-hop-to hop))
          (fons-path-hops path)))

(defun fons-relation (to paths)
  "Return relation to TO among PATHS.
PATHS should be from a single source."
  (let* ((from (fons-hop-from (car (fons-path-hops (car paths)))))
         (paths-to (cl-remove-if-not
                    (lambda (path)
                      (fons-path-to-p to path))
                    paths))
         (relation (make-fons-relation :from from :to to
                                       :paths paths-to)))
    (setf (fons-relation-score relation)
          (funcall fons-relation-score-fn relation))
    relation))

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

(defun fons-path-to-p (to path)
  "Return non-nil if PATH is to TO."
  (equal to (fons-hop-to (car (last (fons-path-hops path))))))

(defun fons-path-circular-p (path)
  "Return non-nil if the last hop in PATH circles back to any earlier hop."
  ;; We only need to check PATH's last hop if this check runs at each iteration.
  (let ((last-hop-to (fons-hop-to (car (last (fons-path-hops path))))))
    (cl-some (lambda (hop)
               (equal last-hop-to (fons-hop-from hop)))
             (fons-path-hops path))))

;; (cl-defun fons-relation (to paths &key (score-fn #'fons-score-patsh))
;;   "Return relation aggregating PATHS to TO.
;; PATHS should be a list of paths from a single source to TO."
;;   ;; TODO: Consider asserting that all PATHS are from the same source.
;;   (let* ((relation (make-fons-relation :from from :to to)))
;;     (setf (fons-relation-score relation)
;;           ;; compute score from paths
;;           )
;;     relation))

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

(defun fons-copy-tree (tree &optional vecp)
  "Copy TREE like `copy-tree', but with VECP, works for records too."
  ;; TODO: Now that the new copy-tree behavior has been merged into Emacs,
  ;; remove this function once compat.el supports the new behavior.
  (if (consp tree)
      (let (result)
	(while (consp tree)
	  (let ((newcar (car tree)))
	    (if (or (consp (car tree))
                    (and vecp (or (vectorp (car tree))
                                  (recordp (car tree)))))
		(setf newcar (fons-copy-tree (car tree) vecp)))
	    (push newcar result))
	  (setf tree (cdr tree)))
	(nconc (nreverse result)
               (if (and vecp (or (vectorp tree) (recordp tree)))
                   (fons-copy-tree tree vecp)
                 tree)))
    (if (and vecp (or (vectorp tree) (recordp tree)))
	(let ((i (length (setf tree (copy-sequence tree)))))
	  (while (>= (setf i (1- i)) 0)
	    (aset tree i (fons-copy-tree (aref tree i) vecp)))
	  tree)
      tree)))

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
