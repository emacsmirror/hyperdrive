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

(defvar fons-blocker-topic "_blocker"
  "Special topic name used for BLOCKER relations.")

(defvar fons-default-topic "_default"
  "Special topic name used as a fallback when no topic is specified.")

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
    (root &key hops-fn topic (blocked (make-hash-table))
          (max-hops 3) (threshold fons-path-score-threshold))
  "Return two hash tables (RELATIONS . BLOCKED-RELATIONS).

HOPS-FN is the function that accepts two arguments, FROM and
TOPIC, and returns a list of `fons-hops' structs.

Each table contains `fons-relation' structs from ROOT about
TOPIC.  Recurses up to MAX-HOPS times, including only relations
whose scores are above THRESHOLD.

BLOCKED may be a hash table keyed by TOs which should not be
recursed into and whose relations will be returned as
BLOCKED-RELATIONS.  The hash table values of BLOCKED are unused,
but they may be a list of BLOCKERs, as in `fons-blocked'."
  (unless (and (integerp max-hops) (cl-plusp max-hops))
    (error "MAX-HOPS must be an positive integer"))
  (when (member root (hash-table-keys blocked))
    (error "BLOCKED must not contain ROOT"))
  (unless topic (setq topic fons-default-topic))
  (let ((relations (make-hash-table :test 'equal))
        (blocked-relations (make-hash-table :test 'equal)))
    (cl-labels ((add-relations-from (from &optional paths-to-from)
                  (dolist (hop (funcall hops-fn from topic))
                    (when-let ((to-relation
                                (and (not (equal root (fons-hop-to hop)))
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
                                 (within-max-hops-p to-relation)
                                 (not (member (fons-hop-to hop)
                                              (hash-table-keys blocked))))
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
                      (puthash to (make-fons-relation :from root :to to)
                               relations))))
      (add-relations-from root)
      (maphash (lambda (to relation)
                 (unless (above-threshold-p relation)
                   (remhash to relations)))
               relations)
      (maphash (lambda (to relation)
                 (when (member to (hash-table-keys blocked))
                   (puthash to relation blocked-relations)
                   (remhash to relations)))
               relations)
      (cons relations blocked-relations))))

(defun fons-direct-blocks (_blocker)
  "Return direct blocks by BLOCKER."
  (error "Not yet implemented (bound in tests)"))

(defun fons-blocked (blockers)
  "Return BLOCKED hash table based on BLOCKERS.
BLOCKERS may be a hash table, keyed by BLOCKER identifier.
BLOCKERS hash values are unused, but may be a list of
`fons-relation' structs, as in the car of the return value of
`fons-relations'.

BLOCKED hash table is keyed by BLOCKED identifier, and each hash
value is a list of BLOCKER identifiers which blocked BLOCKED."
  (let ((blocked (make-hash-table :test #'equal)))
    (dolist (blocker (hash-table-keys blockers))
      (dolist (direct-block (fons-direct-blocks blocker))
        (cl-callf2 push blocker (gethash direct-block blocked))))
    blocked))

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

(defun hyperdrive-fons-relations-hops (relations)
  "Return hops for RELATIONS.
RELATIONS may be a hash table of `fons-relation' structs."
  (let (hops)
    (cl-labels ((map-relation (_to relation)
                  (mapc #'map-path (fons-relation-paths relation)))
                (map-path (path)
                  (mapc #'map-hop (fons-path-hops path)))
                (map-hop (hop)
                  ;; TODO: Benchmark alternative methods to collect hops:
                  ;; 1. push, then delete-dups
                  ;; 2. (unless (cl-find hop hops) (push hop hops))
                  ;; 3. hash table instead of cl-pushnew on a list
                  (cl-pushnew hop hops :test #'equal)))
      (maphash #'map-relation relations)
      hops)))

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
