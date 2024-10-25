;;; hyperdrive-fons.el ---      -*- lexical-binding: t; -*-


;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)

;;;; Types

;; NOTE: If we ever use these as arguments to interactive functions, we should
;; ensure that they are not persisted in `command-history' by `savehist'; see
;; similar workaround in Ement.el.

(cl-defstruct fons-user
  (id nil :documentation "Should uniquely identify the user."
      :type string)
  (blocked nil :type list
           :documentation "List of user IDs that are blocked by this user.")
  (blockers nil :type list
            :documentation "List of user IDs that are blockers for this user.")
  (sources nil :type list
           :documentation "Map keyed by topic with each value being a list of user IDs."))

(cl-defstruct fons-hop from to)
(cl-defstruct fons-path hops)
(cl-defstruct fons-relation from to paths
              (blocked-p nil :documentation "Whether the peer is blocked."
                         :type boolean))

;;;; Variables

(defvar fons-default-topic "_default"
  "Special topic name used as a fallback when no topic is specified.")

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

Each table contains `fons-relation' structs from ROOT (a
`fons-user' struct).  Recurses up to MAX-HOPS times.

BLOCKED may be a hash table keyed by TOs which should not be
recursed into and whose relations will be flagged as blocked.
The hash table values of BLOCKED are unused, but they may be a
list of BLOCKERs, as in `fons-blocked'."
  (unless (and (integerp max-hops) (cl-plusp max-hops))
    (error "MAX-HOPS must be an positive integer"))
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
                  ;; Argument is optional so the callback can be called in case of
                  ;; error to decrement `pending'.
                  (dolist (block blocks)
                    (let* ((hop (make-fons-hop :from blocker :to block))
                           (block-relation
                            (make-fons-relation
                             :from blocker :to block :blocked-p t
                             :paths (list (make-fons-path :hops (list hop))))))
                      (push block-relation (gethash block blocked))))
                  (when (zerop (cl-decf pending))
                    (funcall finally blocked)))))
             blockers)))

(cl-defun fons-blocked-from-p (&key from-id target-id users)
  "Return non-nil if TARGET-ID is blocked from FROM-ID's
perspective.  USERS is a list of `fons-user' structs."
  (let ((from-user (cl-find from-id users :key #'fons-user-id :test #'equal)))
    (or (member target-id (fons-user-blocked from-user))
        (cl-loop for blocker in (fons-user-blockers )
                 thereis (fons-blocked-from-p
                          :from blocker :target target-id :users users))))
  )

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
