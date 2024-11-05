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
(cl-defstruct fons-relation from to paths
              (blocked-p nil :documentation "Whether the peer is blocked."
                         :type boolean))

;;;; Variables

(defgroup fons nil
  "FIXME: Docstring."
  :group 'applications)

;;;; Functions

(cl-defun fons-relations
    (root &key hops-fn topic finally (blocked (make-hash-table)) (max-hops 3))
  "Calculate hash table of relations and call FINALLY.

HOPS-FN is the function that accepts three arguments, FROM,
TOPIC, and a function which should be called asynchronously with
a list of TOs from FROM.

FINALLY is a callback function which will be called with the
relations hash table as its sole argument.

Each table contains `fons-relation' structs from ROOT.  Recurses
up to MAX-HOPS times.

BLOCKED may be a hash table keyed by TOs which should not be
recursed into and whose relations will be flagged as blocked.
The hash table values of BLOCKED are unused, but they may be a
list of BLOCKERs, as in `fons-blocked'."
  (unless (and (integerp max-hops) (cl-plusp max-hops))
    (error "MAX-HOPS must be a positive integer"))
  (when (gethash root blocked)
    (error "BLOCKED must not contain ROOT"))
  (let ((relations (make-hash-table :test 'equal))
        (pending 0))
    (cl-labels
        ((add-relations-from (from &optional paths-to-from)
           (cl-incf pending)
           (funcall
            hops-fn from topic
            (lambda (tos)
              (dolist (to tos)
                (when-let ((hop (make-fons-hop :from from :to to))
                           (to-relation
                            (and (not (equal root to))
                                 (ensure-relation to)))
                           (paths-to-to
                            (if paths-to-from
                                ;; `extended-paths' may return nil if the only
                                ;; paths to TO are circular.
                                (extended-paths paths-to-from hop)
                              ;; On the 1st hop, paths-to-from is nil.
                              (list (make-fons-path :hops (list hop))))))
                  (update-relation to-relation paths-to-to)
                  (when (and (within-max-hops-p to-relation)
                             (not (gethash to blocked)))
                    (add-relations-from (fons-relation-to to-relation)
                                        paths-to-to))))
              (when (zerop (cl-decf pending))
                (maphash (lambda (to relation)
                           (when (gethash to blocked)
                             (setf (fons-relation-blocked-p relation) t)))
                         relations)
                (funcall finally relations)))))
         (update-relation (relation paths)
           (setf (fons-relation-paths relation)
                 (append paths (fons-relation-paths relation))))
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
         (within-max-hops-p (relation)
           (cl-some (lambda (path)
                      (length< (fons-path-hops path) max-hops))
                    (fons-relation-paths relation)))
         (ensure-relation (to)
           "Add relation from ROOT to TO if none exists, then return it."
           (or (gethash to relations)
               (puthash to (make-fons-relation :from root :to to)
                        relations))))
      (add-relations-from root))))

(defun fons-filter-shortest-path (merge-relations)
  "Return MERGE-RELATIONS with only shortest paths."
  (let ((copy-merge-relations (copy-hash-table merge-relations)))
    ;; TODO(review) copying hash table and nested lists
    (cl-labels
        ((map-merge-relation (id merge-relation)
           (thunk-let ((copy-merge-relation
                        (compat-call copy-tree merge-relation t)))
             (dolist (type '(sources blockers))
               ;; Skip `blocked', since its paths are always one hop.
               (when-let*
                   ((relation (map-elt merge-relation type))
                    (shortest-hops-length
                     ;; TODO(review): Merge `cl-loop', `cl-remove-if' for speed?
                     (cl-loop for path in (fons-relation-paths relation)
                              minimize (length (fons-path-hops path))))
                    (new-paths
                     (cl-remove-if (apply-partially #'< shortest-hops-length)
                                   (fons-relation-paths relation)
                                   :key (lambda (path)
                                          (length (fons-path-hops path))))))
                 (unless (eq new-paths (fons-relation-paths relation))
                   (setf (fons-relation-paths
                          (map-elt copy-merge-relation type))
                         new-paths)
                   (setf (gethash id copy-merge-relations)
                         copy-merge-relation)))))))
      (maphash #'map-merge-relation copy-merge-relations)
      copy-merge-relations)))

(cl-defun fons-filter-only-paths-to (ids merge-relations)
  "Return MERGE-RELATIONS with only paths to IDS.
When used in conjunction with `fons-filter-shortest-path',
`fons-filter-only-paths-to' must be applied second, and not vice
versa, to ensure the exclusion of nodes which form a longer path
to one of the IDS."
  (unless ids (cl-return-from fons-filter-only-paths-to merge-relations))
  ;; TODO: Refactor with `cl-loop'.
  (let ((copy-merge-relations (make-hash-table :test 'equal))
        blocker-ids)
    ;; For each ID which is a source, keep the source relation for all IDs which
    ;; are part of a path to it.
    (dolist (id ids)
      (when-let (sources (map-elt (gethash id merge-relations) 'sources))
        (setf (map-elt (gethash id copy-merge-relations) 'sources) sources)
        (dolist (path (fons-relation-paths sources))
          (pcase-dolist ((cl-struct fons-hop from) (fons-path-hops path))
            (when-let* ((merge-relation (gethash from merge-relations))
                        (sources (map-elt merge-relation 'sources)))
              (setf (map-elt (gethash from copy-merge-relations) 'sources)
                    sources))))))
    ;; For each ID which is blocked, add the blocked relation.  Also track the
    ;; `blocker-id's of the blockers which block ID.
    (dolist (id ids)
      (when-let (blocked (map-elt (gethash id merge-relations) 'blocked))
        (setf (map-elt (gethash id copy-merge-relations) 'blocked) blocked)
        (dolist (path (fons-relation-paths blocked))
          (push (fons-hop-from (car (fons-path-hops path))) blocker-ids))))
    ;; For each ID which is a blocker and for each blocker which blocked an ID,
    ;; keep the blocker relation for all IDs which are part of a path to it.
    (dolist (id (delete-dups (append ids blocker-ids)))
      (when-let (blockers (map-elt (gethash id merge-relations) 'blockers))
        (setf (map-elt (gethash id copy-merge-relations) 'blockers) blockers)
        (dolist (path (fons-relation-paths blockers))
          (pcase-dolist ((cl-struct fons-hop from) (fons-path-hops path))
            (when-let* ((merge-relation (gethash from merge-relations))
                        (blockers (map-elt merge-relation 'blockers)))
              (setf (map-elt (gethash from copy-merge-relations) 'blockers)
                    blockers))))))
    copy-merge-relations))

(cl-defun fons-filter-to-types
    (merge-relations &key sourcesp blockersp blockedp all-blocked-p)
  "Return MERGE-RELATIONS based on relation type.
When BLOCKERSP, include \\+`blockers'.

When SOURCESP but not BLOCKEDP, include \\+`sources' for which
`fons-relation-blocked-p' returns nil.

When SOURCESP and BLOCKEDP, include all \\+`sources'.

When BLOCKEDP but not ALL-BLOCKED-P, include \\+`blocked' which
are also \\+`sources'.

When BLOCKEDP and ALL-BLOCKED-P, include all \\+`blocked'."
  (let ((copy-merge-relations (make-hash-table :test 'equal)))
    (maphash
     (lambda (id merge-relation)
       (let ((blocker-relation (map-elt merge-relation 'blockers))
             (source-relation (map-elt merge-relation 'sources))
             (blocked-relation (map-elt merge-relation 'blocked)))
         (when (and blockersp blocker-relation)
           (setf (map-elt (gethash id copy-merge-relations) 'blockers)
                 blocker-relation))
         (when (and sourcesp source-relation
                    (or blockedp (not blocked-relation)))
           (setf (map-elt (gethash id copy-merge-relations) 'sources)
                 source-relation))
         (when (and blockedp
                    blocked-relation
                    (or all-blocked-p source-relation))
           (setf (map-elt (gethash id copy-merge-relations) 'blocked)
                 blocked-relation))))
     merge-relations)
    copy-merge-relations))

(cl-defun fons-blocked (blockers &key blocked-fn finally)
  "Calculate hash table of blocked and call FINALLY.

BLOCKED-FN is a function that accepts two arguments, BLOCKER and
a function which should be called asynchronously with a list of
blocked IDs by BLOCKER.

FINALLY is a callback function which will be called with the
blocked hash table as its sole argument."
  (let ((blocked (make-hash-table :test 'equal))
        (pending (hash-table-count blockers)))
    (maphash (lambda (blocker _)
               (funcall
                blocked-fn blocker
                (lambda (&optional blocks)
                  (dolist (block blocks)
                    (let ((path (make-fons-path
                                 :hops (list (make-fons-hop
                                              :from blocker :to block))))
                          (blocked-relation
                           (or (gethash block blocked)
                               (setf (gethash block blocked)
                                     (make-fons-relation
                                      :from blocker :to block
                                      :blocked-p t :paths nil)))))
                      (push path (fons-relation-paths blocked-relation))))
                  (when (zerop (cl-decf pending))
                    (funcall finally blocked)))))
             blockers)))

(cl-defun fons-merge-relations (sources blockers blocked)
  "Return hash table which merges SOURCES, BLOCKERS, and BLOCKED."
  (let ((merge-relations (make-hash-table :test 'equal)))
    (dolist (table (list sources blockers blocked))
      (maphash (lambda (id relation)
                 (setf (map-elt
                        (gethash id merge-relations)
                        (pcase table
                          ((pred (eq sources)) 'sources)
                          ((pred (eq blockers)) 'blockers)
                          ((pred (eq blocked)) 'blocked)))
                       relation))
               table))
    merge-relations))

(defun fons-merge-relations-hops (merge-relations type)
  "Return hops of TYPE for MERGE-RELATIONS.
MERGE-RELATIONS may be a hash table as in `fons-merge-relations'."
  (let (hops)
    (cl-labels ((map-merge-relation (_to merge-relation)
                  (when-let ((relation (map-elt merge-relation type)))
                    (mapc #'map-path (fons-relation-paths relation))))
                (map-path (path)
                  (mapc #'map-hop (fons-path-hops path)))
                (map-hop (hop)
                  ;; TODO: Benchmark alternative methods to collect hops:
                  ;; 1. push, then delete-dups
                  ;; 2. (unless (cl-find hop hops) (push hop hops))
                  ;; 3. hash table instead of cl-pushnew on a list
                  (cl-pushnew hop hops :test #'equal)))
      (maphash #'map-merge-relation merge-relations)
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
