;;; hyperdrive-sbb.el ---      -*- lexical-binding: t; -*-

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

(cl-defstruct (h/sbb-hop (:constructor h/sbb-hop-create))
  from to)
(cl-defstruct (h/sbb-path (:constructor h/sbb-path-create))
  hops)
(cl-defstruct (h/sbb-relation (:constructor h/sbb-relation-create))
  from to source-paths blocker-paths blocked-paths)

;;;; Variables

(defgroup h/sbb nil
  "Simple structure for gathering sources, blockers, and blocked peers."
  :group 'hyperdrive)

;;;; Functions

(defun h/sbb-relation-paths-of-type (type relation)
  "Return paths of TYPE for RELATION."
  (pcase type
    ('sources (h/sbb-relation-source-paths relation))
    ('blockers (h/sbb-relation-blocker-paths relation))
    ('blocked (h/sbb-relation-blocked-paths relation))))

(gv-define-setter h/sbb-relation-paths-of-type (paths type relation)
  `(pcase ,type
     ('sources (setf (h/sbb-relation-source-paths ,relation) ,paths))
     ('blockers (setf (h/sbb-relation-blocker-paths ,relation) ,paths))
     ('blocked (setf (h/sbb-relation-blocked-paths ,relation) ,paths))))

(cl-defun h/sbb-relations
    ( root &key (relations (make-hash-table :test 'equal))
      hops-fn type finally (max-hops 3))
  "Calculate hash table of relations and call FINALLY.

Table is keyed by `hyperdrive-sbb-relation-to' and its values are
`hyperdrive-sbb-relation' structs from ROOT.

RELATIONS may be nil or an existing RELATIONS hash table to use.
If a relation in RELATIONS has
`hyperdrive-sbb-relation-blocked-paths', its
`hyperdrive-sbb-relation-to' will not be recursed into.

HOPS-FN is the function that accepts two arguments, FROM and a
function which should be called asynchronously with a list of TOs
from FROM.  Recurses up to MAX-HOPS times.

With TYPE \\+`sources', add paths to
`hyperdrive-sbb-relation-source-paths'.  With TYPE \\+`blockers',
add paths to `hyperdrive-sbb-relation-blocker-paths'.

FINALLY is a callback function which will be called with the
relations hash table as its sole argument."
  (declare (indent defun))
  (when (zerop max-hops)
    (cl-return-from h/sbb-relations (funcall finally relations)))
  (unless (and (integerp max-hops) (cl-plusp max-hops))
    (error "MAX-HOPS for TYPE `%s' must be a non-negative integer" type))
  (when-let* ((root-relation (gethash root relations))
              ((h/sbb-relation-blocked-paths root-relation)))
    (error "ROOT must not be blocked"))
  (let ((pending-relations 0))
    (cl-labels
        ((add-relations-from (from &optional paths-to-from)
           (cl-incf pending-relations)
           (funcall
            hops-fn from
            (lambda (tos)
              (dolist (to tos)
                (when-let ((hop (h/sbb-hop-create :from from :to to))
                           ((not (equal root to)))
                           (to-relation
                            (and (not (equal root to))
                                 (ensure-relation to)))
                           (paths-to-to (extended-paths paths-to-from hop)))
                  (cl-callf append
                      (h/sbb-relation-paths-of-type type to-relation)
                    paths-to-to)
                  (when (and (within-max-hops-p to-relation)
                             (not (h/sbb-relation-blocked-paths
                                   (gethash to relations))))
                    (add-relations-from (h/sbb-relation-to to-relation)
                                        paths-to-to))))
              (when (zerop (cl-decf pending-relations))
                (funcall finally relations)))))
         (extended-paths (paths hop)
           "Return list of PATHS extended by HOP without circular hops."
           (if paths
               (cl-loop
                for path in paths
                unless (circular-p path hop)
                collect (h/sbb-path-create
                         :hops (append (h/sbb-path-hops path) (list hop))))
             (list (h/sbb-path-create :hops (list hop)))))
         (circular-p (path last-hop)
           "Return non-nil when HOP circles back to any hop in PATH."
           (cl-some (lambda (hop)
                      (equal (h/sbb-hop-to last-hop) (h/sbb-hop-from hop)))
                    (h/sbb-path-hops path)))
         (within-max-hops-p (relation)
           (cl-some (lambda (path)
                      (length< (h/sbb-path-hops path) max-hops))
                    (h/sbb-relation-paths-of-type type relation)))
         (ensure-relation (to)
           "Add relation from ROOT to TO if none exists, then return it."
           (or (gethash to relations)
               (puthash to (h/sbb-relation-create :from root :to to) relations))))
      (add-relations-from root))))

(cl-defun h/sbb-blocked (root relations &key hops-fn finally)
  "Add blocked to RELATIONS hash table and call FINALLY.

Add blocked for each relation in RELATIONS which has non-nil
\\+`blocker-paths'.  HOPS-FUN is a function that accepts two
arguments, BLOCKER and a function which should be called
asynchronously with a list of blocked IDs by BLOCKER.

FINALLY is a callback function which will be called with the
updated RELATIONS hash table as its sole argument."
  (declare (indent defun))
  (let ((pending-relations (1+ (hash-table-count relations))))
    (cl-labels
        ((map-relation (id relation)
           (if (h/sbb-relation-blocker-paths relation)
               (add-blocked-for id)
             (cl-decf pending-relations)))
         (add-blocked-for (id)
           (funcall
            hops-fn id
            (lambda (&optional blocks)
              (dolist (block blocks)
                (let ((path (h/sbb-path-create :hops (list (h/sbb-hop-create
                                                            :from id :to block))))
                      (blocked-relation
                       (or (gethash block relations)
                           (setf (gethash block relations)
                                 (h/sbb-relation-create :from id :to block)))))
                  (push path (h/sbb-relation-blocked-paths blocked-relation))))
              (when (zerop (cl-decf pending-relations))
                (funcall finally relations))))))
      (add-blocked-for root)
      (maphash #'map-relation relations))))

(defun h/sbb-filter-shortest-paths (relations root)
  "Return RELATIONS with only shortest paths.
ROOT is used to determine the shortest blocked relation."
  ;; Call `h/sbb-filter-shortest-paths' before `h/sbb-filter-to-types' since
  ;; getting the shortest blocked path requires knowing blocker paths.
  (let ((copy-relations (copy-hash-table relations))
        (blocker-hops-cache (make-hash-table :test 'equal)))
    ;; TODO(review) copying hash table and nested lists
    (cl-labels
        ((map-relation (id relation)
           (thunk-let ((copy-relation (compat-call copy-tree relation t)))
             (dolist (type '(sources blockers))
               (when-let* ((paths (h/sbb-relation-paths-of-type type relation))
                           (shortest-paths (shortest-paths paths)))
                 (unless (eq shortest-paths paths)
                   (setf (h/sbb-relation-paths-of-type type copy-relation)
                         shortest-paths)
                   (setf (gethash id copy-relations) copy-relation))))
             (when-let* ((paths (h/sbb-relation-paths-of-type
                                 'blocked relation))
                         (shortest-paths (shortest-blocked-paths paths)))
               (unless (eq shortest-paths paths)
                 (setf (h/sbb-relation-paths-of-type 'blocked relation)
                       shortest-paths)
                 (setf (gethash id copy-relations) copy-relation)))))
         (shortest-hops-length (paths)
           (cl-loop for path in paths
                    minimize (length (h/sbb-path-hops path))))
         (shortest-paths (paths)
           (cl-remove-if
            (apply-partially #'< (shortest-hops-length paths)) paths
            :key (lambda (path) (length (h/sbb-path-hops path)))))
         (shortest-blocked-paths (paths)
           ;; TODO: There must be a better way to write this to not loop 3x.
           (if-let ((direct-block-path
                     (cl-find-if (lambda (id) (equal root id))
                                 paths :key #'h/sbb-blocked-path-blocker)))
               (list direct-block-path)
             (let ((shortest-blocker-hops-length
                    (cl-loop for path in paths
                             minimize (hops-to-blocker
                                       (h/sbb-blocked-path-blocker path)))))
               (cl-remove-if
                (apply-partially #'< shortest-blocker-hops-length) paths
                :key (lambda (path)
                       (hops-to-blocker (h/sbb-blocked-path-blocker path)))))))
         (hops-to-blocker (id)
           (with-memoization (gethash id blocker-hops-cache)
             ;; Blockers are already filtered to shortest paths.
             ;; All paths have the same, shortest length.
             (length (h/sbb-path-hops (car (h/sbb-relation-blocker-paths
                                            (gethash id copy-relations))))))))
      (maphash #'map-relation copy-relations)
      copy-relations)))

(cl-defun h/sbb-filter-paths-only-to (ids relations)
  "Return RELATIONS with only paths to IDS.
`hyperdrive-sbb-filter-paths-only-to' must be applied after other
filter functions to ensure the exclusion of nodes which form a
longer path to one of the IDS."
  (unless ids (cl-return-from h/sbb-filter-paths-only-to relations))
  ;; TODO: Refactor with `cl-loop'.
  (let ((copy-relations (make-hash-table :test 'equal))
        ;; Remove IDS which aren't in RELATIONS anyway.
        (relation-ids (cl-remove-if-not
                       (lambda (id) (gethash id relations))
                       ids))
        blocker-ids)
    (cl-labels ((safe-copy-relation (id relation)
                  (or (gethash id copy-relations)
                      (h/sbb-relation-create :from (h/sbb-relation-from relation)
                                             :to (h/sbb-relation-to relation)))))
      ;; For each ID which is a source, keep the source paths for all IDs which
      ;; are part of a path to it.
      (dolist (id relation-ids)
        (dolist (path (h/sbb-relation-source-paths (gethash id relations)))
          (pcase-dolist ((cl-struct h/sbb-hop to) (h/sbb-path-hops path))
            (when-let* ((relation (gethash to relations))
                        (blocker-paths (h/sbb-relation-source-paths relation))
                        (copy-relation (safe-copy-relation to relation)))
              (setf (h/sbb-relation-source-paths copy-relation) blocker-paths)
              (setf (gethash to copy-relations) copy-relation)))))
      ;; For each ID which is blocked, add the blocked relation.  Also track the
      ;; `blocker-id's of the blockers which block ID.
      (dolist (id relation-ids)
        (when-let ((relation (gethash id relations))
                   (blocked-paths (h/sbb-relation-blocked-paths relation))
                   (copy-relation (safe-copy-relation id relation)))
          (setf (h/sbb-relation-blocked-paths copy-relation) blocked-paths)
          (setf (gethash id copy-relations) copy-relation)
          (dolist (path blocked-paths)
            (let ((blocker-id (h/sbb-blocked-path-blocker path)))
              (when (gethash blocker-id relations)
                ;; `blocker-id' may not be in RELATIONS if it is root or if
                ;; RELATIONS has been filtered to exclude BLOCKERS.
                (push blocker-id blocker-ids))))))
      ;; For each ID which is a blocker and for each blocker which blocked an ID,
      ;; keep the blocker relation for all IDs which are part of a path to it.
      (dolist (id (delete-dups (append relation-ids blocker-ids)))
        (dolist (path (h/sbb-relation-blocker-paths (gethash id relations)))
          (pcase-dolist ((cl-struct h/sbb-hop to) (h/sbb-path-hops path))
            (when-let* ((relation (gethash to relations))
                        (blocker-paths (h/sbb-relation-blocker-paths relation))
                        (copy-relation (safe-copy-relation to relation)))
              (setf (h/sbb-relation-blocker-paths copy-relation) blocker-paths)
              (setf (gethash to copy-relations) copy-relation)))))
      copy-relations)))

(cl-defun h/sbb-filter-to-types
    (relations &key sourcesp blockersp blocked-sources-p blocked-non-sources-p)
  "Return RELATIONS based on relation type.
When BLOCKERSP, include \\+`blocker-paths'.

When SOURCESP, include \\+`source-paths' if either
BLOCKED-SOURCES-P or the relation has no \\+`blocked-paths'.

When BLOCKED-SOURCES-P, include \\+`blocked-paths' if relation
also has \\+`source-paths'.

When BLOCKED-NON-SOURCES-P, include \\+`blocked-paths' if relation
has no \\+`source-paths'."
  (let ((copy-relations (make-hash-table :test 'equal)))
    (maphash
     (lambda (id relation)
       (let ((blocker-paths (h/sbb-relation-blocker-paths relation))
             (source-paths (h/sbb-relation-source-paths relation))
             (blocked-paths (h/sbb-relation-blocked-paths relation))
             (copy-relation (h/sbb-relation-create
                             :from (h/sbb-relation-from relation)
                             :to (h/sbb-relation-to relation))))
         (when (and blockersp blocker-paths)
           (setf (h/sbb-relation-blocker-paths copy-relation) blocker-paths)
           (setf (gethash id copy-relations) copy-relation))
         (when (and sourcesp source-paths
                    (or blocked-sources-p (not blocked-paths)))
           (setf (h/sbb-relation-source-paths copy-relation) source-paths)
           (setf (gethash id copy-relations) copy-relation))
         (when (and blocked-paths
                    (or (and blocked-sources-p source-paths)
                        (and blocked-non-sources-p (null source-paths))))
           (setf (h/sbb-relation-blocked-paths copy-relation) blocked-paths)
           (setf (gethash id copy-relations) copy-relation))))
     relations)
    copy-relations))

(defun h/sbb-relations-hops (relations type)
  "Return hops of TYPE for RELATIONS.
RELATIONS may be a hash table of `hyperdrive-sbb-relations' structs."
  (let (hops)
    (cl-labels ((map-relation (_to relation)
                  (mapc #'map-path (h/sbb-relation-paths-of-type type relation)))
                (map-path (path)
                  (mapc #'map-hop (h/sbb-path-hops path)))
                (map-hop (hop)
                  ;; TODO: Benchmark alternative methods to collect hops:
                  ;; 1. push, then delete-dups
                  ;; 2. (unless (cl-find hop hops) (push hop hops))
                  ;; 3. hash table instead of cl-pushnew on a list
                  (cl-pushnew hop hops :test #'equal)))
      (maphash #'map-relation relations)
      hops)))

(defun h/sbb-relation-direct-p (root type relation)
  "Return non-nil if RELATION is direct from ROOT for TYPE."
  (cl-loop for path in (h/sbb-relation-paths-of-type type relation)
           thereis
           (equal root (h/sbb-hop-from (car (last (h/sbb-path-hops path)))))))

(defun h/sbb-blocked-path-blocker (blocked-path)
  "Return blocker id for BLOCKED-PATH."
  (h/sbb-hop-from (car (h/sbb-path-hops blocked-path))))

(defun h/sbb-shortest-hops-length (type relation)
  "Return the minimum number of TYPE hops in RELATION."
  (cl-loop for path in (h/sbb-relation-paths-of-type type relation)
           minimize (length (h/sbb-path-hops path))))

(cl-defun h/sbb-shortest-blocked-hops-length (relation relations root)
  "Return the minimum number of blocked hops in RELATION.
A blocked hop includes the number of hops to the blocker based on
RELATIONS.  If one of the blockers in RELATION is ROOT, return 1."
  (when-let*
      ((blocked-paths (h/sbb-relation-blocked-paths relation))
       (blockers
        (mapcar (lambda (path)
                  (let ((blocker-id (h/sbb-blocked-path-blocker path)))
                    (when (equal root blocker-id)
                      ;; Direct block from root: return 1.
                      (cl-return-from h/sbb-shortest-blocked-hops-length 1))
                    (gethash blocker-id relations)))
                blocked-paths)))
    (1+ (cl-loop
         for blocker in blockers
         minimize (h/sbb-shortest-hops-length 'blockers blocker)))))

;;;; Footer

(provide 'hyperdrive-sbb)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:

;;; hyperdrive-sbb.el ends here
