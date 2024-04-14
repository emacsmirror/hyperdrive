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

(defvar fons-blocker-topic "_blocker"
  "Special topic name used for BLOCKER relations.")

(defvar fons-default-topic "_default"
  "Special topic name used as a fallback when no topic is specified.")

(defgroup fons nil
  "FIXME: Docstring."
  :group 'applications)

;;;; Functions

(cl-defun fons-relations
    (root &key hops-fn (blocked (make-hash-table)) (max-hops 3))
  "Return hash table of relations.

HOPS-FN is the function that accepts the argument FROM and
returns a list of `fons-hops' structs.

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
  (let ((relations (make-hash-table :test 'equal)))
    (cl-labels ((add-relations-from (from &optional paths-to-from)
                  ;; TODO: Consider returning a list of TOs instead of hops.
                  (dolist (hop (funcall hops-fn from))
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
                      (update-relation to-relation paths-to-to)
                      (when (and (within-max-hops-p to-relation)
                                 (not (gethash (fons-hop-to hop) blocked)))
                        (add-relations-from (fons-relation-to to-relation)
                                            paths-to-to)))))
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
      (add-relations-from root)
      (maphash (lambda (to relation)
                 (when (gethash to blocked)
                   (setf (fons-relation-blocked-p relation) t)))
               relations)
      relations)))

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
