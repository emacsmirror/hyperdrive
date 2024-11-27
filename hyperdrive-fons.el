;;; hyperdrive-fons.el ---      -*- lexical-binding: t; -*-

;;; Commentary:

;; This library implements functions for working with sources, blockers, and
;; blocked.  This data model is useful for exploring a network of sources of
;; information, such as in a peer-to-peer application like `hyperdrive'.

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
    ( root &key (relations (make-hash-table :test 'equal))
      hops-fn type finally (max-hops 3))
  "Calculate hash table of relations and call FINALLY.

Table is keyed by `fons-relation-to' and its values are
`fons-relation' structs from ROOT.

RELATIONS may be nil or an existing RELATIONS hash table to use.
If a relation in RELATIONS has `fons-relation-blocked-paths', its
`fons-relation-to' will not be recursed into.

HOPS-FN is the function that accepts two arguments, FROM and a
function which should be called asynchronously with a list of TOs
from FROM.  Recurses up to MAX-HOPS times.

With TYPE \\+`sources', add paths to
`fons-relation-source-paths'.  With TYPE \\+`blockers', add paths
to `fons-relation-blocker-paths'.

FINALLY is a callback function which will be called with the
relations hash table as its sole argument."
  (declare (indent defun))
  (when (zerop max-hops)
    (cl-return-from fons-relations (funcall finally relations)))
  (unless (and (integerp max-hops) (cl-plusp max-hops))
    (error "MAX-HOPS for TYPE `%s' must be a non-negative integer" type))
  (when-let* ((root-relation (gethash root relations))
              ((fons-relation-blocked-paths root-relation)))
    (error "ROOT must not be blocked"))
  (let ((pending-relations 0))
    (cl-labels
        ((add-relations-from (from &optional paths-to-from)
           (cl-incf pending-relations)
           (funcall
            hops-fn from
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
              (when (zerop (cl-decf pending-relations)) (funcall finally relations)))))
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

(cl-defun fons-blocked (root relations &key hops-fn finally)
  "Add blocked to RELATIONS hash table and call FINALLY.

Add blocked for each relation in RELATIONS which has non-nil
\\+`blocker-paths'.  HOPS-FUN is a function that accepts two
arguments, BLOCKER and a function which should be called
asynchronously with a list of blocked IDs by BLOCKER.

FINALLY is a callback function which will be called with the
updated RELATIONS hash table as its sole argument."
  (declare (indent defun))
  (let ((pending-relations (1+ (hash-table-count relations))))
    (cl-labels ((map-relation (id relation)
                  (if (fons-relation-blocker-paths relation)
                      (add-blocked-for id)
                    (cl-decf pending-relations)))
                (add-blocked-for (id)
                  (funcall
                   hops-fn id
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
                     (when (zerop (cl-decf pending-relations))
                       (funcall finally relations))))))
      (add-blocked-for root)
      (maphash #'map-relation relations))))

(defun fons-filter-shortest-path (relations root)
  "Return RELATIONS with only shortest paths.
ROOT is used to determine the shortest blocked relation."
  ;; Call `fons-filter-shortest-path' before `fons-filter-to-types' since
  ;; getting the shortest blocked path requires knowing blocker paths.
  (let ((copy-relations (copy-hash-table relations))
        (blocker-hops-cache (make-hash-table :test 'equal)))
    ;; TODO(review) copying hash table and nested lists
    (cl-labels
        ((map-relation (id relation)
           (thunk-let ((copy-relation (compat-call copy-tree relation t)))
             (dolist (type '(sources blockers))
               (when-let* ((paths (fons-relation-paths-of-type type relation))
                           (shortest-paths (shortest-paths paths)))
                 (unless (eq shortest-paths paths)
                   (setf (fons-relation-paths-of-type type copy-relation)
                         shortest-paths)
                   (setf (gethash id copy-relations) copy-relation))))
             (when-let* ((paths (fons-relation-paths-of-type 'blocked relation))
                         (shortest-paths (shortest-blocked-paths paths)))
               (unless (eq shortest-paths paths)
                 (setf (fons-relation-paths-of-type 'blocked relation)
                       shortest-paths)
                 (setf (gethash id copy-relations) copy-relation)))))
         (shortest-hops-length (paths)
           (cl-loop for path in paths
                    minimize (length (fons-path-hops path))))
         (shortest-paths (paths)
           (cl-remove-if
            (apply-partially #'< (shortest-hops-length paths)) paths
            :key (lambda (path) (length (fons-path-hops path)))))
         (shortest-blocked-paths (paths)
           ;; TODO: There must be a better way to write this to not loop 3x.
           (if-let ((direct-block-path
                     (cl-find-if (lambda (id) (equal root id))
                                 paths :key #'fons-blocked-path-blocker)))
               (list direct-block-path)
             (let ((shortest-blocker-hops-length
                    (cl-loop for path in paths
                             minimize (hops-to-blocker
                                       (fons-blocked-path-blocker path)))))
               (cl-remove-if
                (apply-partially #'< shortest-blocker-hops-length) paths
                :key (lambda (path)
                       (hops-to-blocker (fons-blocked-path-blocker path)))))))
         (hops-to-blocker (id)
           (with-memoization (gethash id blocker-hops-cache)
             ;; Blockers are already filtered to shortest paths.
             ;; All paths have the same, shortest length.
             (length (fons-path-hops (car (fons-relation-blocker-paths
                                           (gethash id copy-relations))))))))
      (maphash #'map-relation copy-relations)
      copy-relations)))

(cl-defun fons-filter-paths-only-to (ids relations root)
  "Return RELATIONS with only paths to IDS.
`fons-filter-paths-only-to' must be applied after other filter
functions to ensure the exclusion of nodes which form a longer
path to one of the IDS.  Avoids attempting to find paths to ROOT."
  (unless ids (cl-return-from fons-filter-paths-only-to relations))
  ;; TODO: Refactor with `cl-loop'.
  (let ((copy-relations (make-hash-table :test 'equal))
        ;; Remove IDS which aren't in RELATIONS anyway.
        (relation-ids (cl-remove-if-not
                       (lambda (id) (gethash id relations))
                       ids))
        blocker-ids)
    (cl-labels ((safe-copy-relation (id relation)
                  (or (gethash id copy-relations)
                      (make-fons-relation :from (fons-relation-from relation)
                                          :to (fons-relation-to relation)))))
      ;; For each ID which is a source, keep the source paths for all IDs which
      ;; are part of a path to it.
      (dolist (id relation-ids)
        (dolist (path (fons-relation-source-paths (gethash id relations)))
          (pcase-dolist ((cl-struct fons-hop to) (fons-path-hops path))
            (when-let* ((relation (gethash to relations))
                        (blocker-paths (fons-relation-source-paths relation))
                        (copy-relation (safe-copy-relation to relation)))
              (setf (fons-relation-source-paths copy-relation) blocker-paths)
              (setf (gethash to copy-relations) copy-relation)))))
      ;; For each ID which is blocked, add the blocked relation.  Also track the
      ;; `blocker-id's of the blockers which block ID.
      (dolist (id relation-ids)
        (when-let ((relation (gethash id relations))
                   (blocked-paths (fons-relation-blocked-paths relation))
                   (copy-relation (safe-copy-relation id relation)))
          (setf (fons-relation-blocked-paths copy-relation) blocked-paths)
          (setf (gethash id copy-relations) copy-relation)
          (dolist (path blocked-paths)
            (let ((blocker-id (fons-blocked-path-blocker path)))
              (unless (equal root blocker-id)
                ;; Don't attempt to filter to paths to root.
                (push blocker-id blocker-ids))))))
      ;; For each ID which is a blocker and for each blocker which blocked an ID,
      ;; keep the blocker relation for all IDs which are part of a path to it.
      (dolist (id (delete-dups (append relation-ids blocker-ids)))
        (dolist (path (fons-relation-blocker-paths (gethash id relations)))
          (pcase-dolist ((cl-struct fons-hop to) (fons-path-hops path))
            (when-let* ((relation (gethash to relations))
                        (blocker-paths (fons-relation-blocker-paths relation))
                        (copy-relation (safe-copy-relation to relation)))
              (setf (fons-relation-blocker-paths copy-relation) blocker-paths)
              (setf (gethash to copy-relations) copy-relation)))))
      copy-relations)))

(cl-defun fons-filter-to-types
    (relations &key sourcesp blockersp blockedp)
  "Return RELATIONS based on relation type.
When BLOCKERSP, include \\+`blocker-paths'.

With non-nil SOURCESP and nil BLOCKEDP, include \\+`source-paths'
if relation has no \\+`blocked-paths'.

With non-nil SOURCESP and non-nil BLOCKEDP, include all
\\+`source-paths'.

If BLOCKEDP is \\+`sources', include \\+`blocked-paths' if
relation also has \\+`source-paths'.  If BLOCKEDP is \\+`all',
include all \\+`blocked-paths'."
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
         (when (and blocked-paths
                    (or (eq blockedp 'all)
                        (and (eq blockedp 'sources) source-paths)))
           (setf (fons-relation-blocked-paths copy-relation) blocked-paths)
           (setf (gethash id copy-relations) copy-relation))))
     relations)
    copy-relations))

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

(defun fons-relation-direct-p (root type relation)
  "Return non-nil if RELATION is direct from ROOT for TYPE."
  (cl-loop for path in (fons-relation-paths-of-type type relation)
           thereis (equal (h/public-key root)
                          (fons-hop-from (car (last (fons-path-hops path)))))))

(defun fons-blocked-path-blocker (blocked-path)
  "Return blocker id for BLOCKED-PATH."
  (fons-hop-from (car (fons-path-hops blocked-path))))

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
