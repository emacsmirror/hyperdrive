;;; hyperdrive-fons.el ---      -*- lexical-binding: t; -*-


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'thunk)
(require 'compat)

;;;; Types

;; NOTE: If we ever use these as arguments to interactive functions, we should
;; ensure that they are not persisted in `command-history' by `savehist'; see
;; similar workaround in Ement.el.

(cl-defstruct fons-hop from to)
(cl-defstruct fons-path hops)
(cl-defstruct fons-relation from to source-paths blocker-paths blocked-paths)

;;;; Variables

(defgroup fons nil
  "FIXME: Docstring."
  :group 'applications)

;;;; Functions

(defun fons-relation-paths-of-type (type relation)
  "Return paths of TYPE for RELATION."
  (pcase type
    ('sources (fons-relation-source-paths relation))
    ('blockers (fons-relation-blocker-paths relation))
    ('blocked (fons-relation-blocked-paths relation))))

(gv-define-setter fons-relation-paths-of-type (paths type relation)
  `(pcase ,type
     ('sources (setf (fons-relation-source-paths ,relation) ,paths))
     ('blockers (setf (fons-relation-blocker-paths ,relation) ,paths))
     ('blocked (setf (fons-relation-blocked-paths ,relation) ,paths))))

(cl-defun fons-relations
    (root &key relations hops-fn topic type finally (max-hops 3))
  "Calculate hash table of relations and call FINALLY.

Table is keyed by `fons-relation-to' and its values are
`fons-relation' structs from ROOT.

RELATIONS may be nil or an existing RELATIONS hash table to use.
If a relation in RELATIONS has `fons-relation-blocked-paths', its
`fons-relation-to' will not be recursed into.

HOPS-FN is the function that accepts three arguments, FROM,
TOPIC, and a function which should be called asynchronously with
a list of TOs from FROM.  Recurses up to MAX-HOPS times.

With TYPE \\+`sources', add paths to `fons-relation-source-paths'.
With TYPE \\+`blockers', add paths to `fons-relation-blocker-paths'.

FINALLY is a callback function which will be called with the
relations hash table as its sole argument."
  (unless (and (integerp max-hops) (cl-plusp max-hops))
    (error "MAX-HOPS must be a positive integer"))
  (let ((relations (or relations (make-hash-table :test 'equal)))
        (pending 0))
    (when-let* ((root-relation (gethash root relations))
                ((fons-relation-blocked-paths root-relation)))
      (error "ROOT must not be blocked"))
    (cl-labels
        ((add-relations-from (from &optional paths-to-from)
           (cl-incf pending)
           (funcall
            hops-fn from topic
            (lambda (tos)
              (dolist (to tos)
                (when-let ((hop (make-fons-hop :from from :to to))
                           ((not (equal root to)))
                           (to-relation
                            (and (not (equal root to))
                                 (ensure-relation to)))
                           (paths-to-to (extended-paths paths-to-from hop)))
                  (cl-callf append (fons-relation-paths-of-type type to-relation)
                    paths-to-to)
                  (when (and (within-max-hops-p to-relation)
                             (not (fons-relation-blocked-paths
                                   (gethash to relations))))
                    (add-relations-from (fons-relation-to to-relation)
                                        paths-to-to))))
              (when (zerop (cl-decf pending)) (funcall finally relations)))))
         (extended-paths (paths hop)
           "Return list of PATHS extended by HOP without circular hops."
           (if paths
               (cl-loop
                for path in paths
                unless (circular-p path hop)
                collect (make-fons-path
                         :hops (append (fons-path-hops path) (list hop))))
             (list (make-fons-path :hops (list hop)))))
         (circular-p (path last-hop)
           "Return non-nil when HOP circles back to any hop in PATH."
           (cl-some (lambda (hop)
                      (equal (fons-hop-to last-hop) (fons-hop-from hop)))
                    (fons-path-hops path)))
         (within-max-hops-p (relation)
           (cl-some (lambda (path)
                      (length< (fons-path-hops path) max-hops))
                    (fons-relation-paths-of-type type relation)))
         (ensure-relation (to)
           "Add relation from ROOT to TO if none exists, then return it."
           (or (gethash to relations)
               (puthash to (make-fons-relation :from root :to to) relations))))
      (add-relations-from root))))

(defun fons-filter-shortest-path (relations)
  "Return RELATIONS with only shortest paths."
  (let ((copy-relations (copy-hash-table relations)))
    ;; TODO(review) copying hash table and nested lists
    (cl-labels
        ((map-relation (id relation)
           (thunk-let ((copy-relation (compat-call copy-tree relation t)))
             (dolist (type '(sources blockers))
               ;; Skip `blocked', since its paths are always one hop.
               (when-let*
                   ((paths (fons-relation-paths-of-type type relation))
                    (shortest-hops-length
                     (cl-loop for path in paths
                              minimize (length (fons-path-hops path))))
                    (new-paths
                     (cl-remove-if
                      (apply-partially #'< shortest-hops-length) paths
                      :key (lambda (path) (length (fons-path-hops path))))))
                 (unless (eq new-paths paths)
                   (setf (fons-relation-paths-of-type type copy-relation)
                         new-paths)
                   (setf (gethash id copy-relations)
                         copy-relation)))))))
      (maphash #'map-relation copy-relations)
      copy-relations)))

(cl-defun fons-filter-only-paths-to (ids relations)
  "Return RELATIONS with only paths to IDS.
`fons-filter-only-paths-to' must be applied after other filter
functions to ensure the exclusion of nodes which form a longer
path to one of the IDS."
  (unless ids (cl-return-from fons-filter-only-paths-to relations))
  ;; TODO: Refactor with `cl-loop'.
  (let ((copy-relations (make-hash-table :test 'equal))
        blocker-ids)
    (cl-labels ((safe-copy-relation (id relation)
                  (or (gethash id copy-relations)
                      (make-fons-relation :from (fons-relation-from relation)
                                          :to (fons-relation-to relation)))))
      ;; For each ID which is a source, keep the source paths for all IDs which
      ;; are part of a path to it.
      (dolist (id ids)
        (dolist (path (fons-relation-source-paths (gethash id relations)))
          (pcase-dolist ((cl-struct fons-hop to) (fons-path-hops path))
            (when-let* ((relation (gethash to relations))
                        (source-paths (fons-relation-source-paths relation))
                        (copy-relation (safe-copy-relation to relation)))
              (setf (fons-relation-source-paths copy-relation) source-paths)
              (setf (gethash to copy-relations) copy-relation)))))
      ;; For each ID which is blocked, add the blocked relation.  Also track the
      ;; `blocker-id's of the blockers which block ID.
      (dolist (id ids)
        (when-let ((relation (gethash id relations))
                   (blocked-paths (fons-relation-blocked-paths relation))
                   (copy-relation (safe-copy-relation id relation)))
          (setf (fons-relation-blocked-paths copy-relation) blocked-paths)
          (setf (gethash id copy-relations) copy-relation)
          (dolist (path blocked-paths)
            (push (fons-hop-from (car (fons-path-hops path))) blocker-ids))))
      ;; For each ID which is a blocker and for each blocker which blocked an ID,
      ;; keep the blocker relation for all IDs which are part of a path to it.
      (dolist (id (delete-dups (append ids blocker-ids)))
        (dolist (path (fons-relation-blocker-paths (gethash id relations)))
          (pcase-dolist ((cl-struct fons-hop to) (fons-path-hops path))
            (when-let* ((relation (gethash to relations))
                        (blocker-paths (fons-relation-blocker-paths relation))
                        (copy-relation (safe-copy-relation to relation)))
              (setf (fons-relation-blocker-paths copy-relation) blocker-paths)
              (setf (gethash to copy-relations) copy-relation)))))
      copy-relations)))

(cl-defun fons-filter-to-types
    (relations &key sourcesp blockersp blockedp all-blocked-p)
  "Return RELATIONS based on relation type.
When BLOCKERSP, include \\+`blocker-paths'.

When SOURCESP but not BLOCKEDP, include \\+`source-paths' if
relation has no \\+`blocked-paths'.

When SOURCESP and BLOCKEDP, include all \\+`source-paths'.

When BLOCKEDP but not ALL-BLOCKED-P, include \\+`blocked-paths'
if relation also has \\+`source-paths'.

When BLOCKEDP and ALL-BLOCKED-P, include all \\+`blocked'."
  (let ((copy-relations (make-hash-table :test 'equal)))
    (maphash
     (lambda (id relation)
       (let ((blocker-paths (fons-relation-blocker-paths relation))
             (source-paths (fons-relation-source-paths relation))
             (blocked-paths (fons-relation-blocked-paths relation))
             (copy-relation (make-fons-relation
                             :from (fons-relation-from relation)
                             :to (fons-relation-to relation))))
         (when (and blockersp blocker-paths)
           (setf (fons-relation-blocker-paths copy-relation) blocker-paths)
           (setf (gethash id copy-relations) copy-relation))
         (when (and sourcesp source-paths (or blockedp (not blocked-paths)))
           (setf (fons-relation-source-paths copy-relation) source-paths)
           (setf (gethash id copy-relations) copy-relation))
         (when (and blockedp blocked-paths (or all-blocked-p source-paths))
           (setf (fons-relation-blocked-paths copy-relation) blocked-paths)
           (setf (gethash id copy-relations) copy-relation))))
     relations)
    copy-relations))

(cl-defun fons-blocked (relations &key blocked-fn finally)
  "Add blocked to RELATIONS hash table and call FINALLY.

Add blocked for each relation in RELATIONS which has non-nil
\\+`blocker-paths'.  BLOCKED-FN is a function that accepts two
arguments, BLOCKER and a function which should be called
asynchronously with a list of blocked IDs by BLOCKER.

FINALLY is a callback function which will be called with the
updated RELATIONS hash table as its sole argument."
  (let ((pending (hash-table-count relations)))
    (maphash (lambda (id relation)
               (if (fons-relation-blocker-paths relation)
                   (funcall
                    blocked-fn id
                    (lambda (&optional blocks)
                      (dolist (block blocks)
                        (let ((path (make-fons-path
                                     :hops (list (make-fons-hop
                                                  :from id :to block))))
                              (blocked-relation
                               (or (gethash block relations)
                                   (setf (gethash block relations)
                                         (make-fons-relation
                                          :from id :to block)))))
                          (push path (fons-relation-blocked-paths blocked-relation))))
                      (when (zerop (cl-decf pending))
                        (funcall finally relations))))
                 (cl-decf pending)))
             relations)))

(defun fons-relations-hops (relations type)
  "Return hops of TYPE for RELATIONS.
RELATIONS may be a hash table of `fons-relations' structs."
  (let (hops)
    (cl-labels ((map-relation (_to relation)
                  (mapc #'map-path (fons-relation-paths-of-type type relation)))
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
