;;; hyperdrive-fons-view.el --- Visualize fons relations  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 USHIN, Inc.

;; Author: Joseph Turner <joseph@ushin.org>
;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Joseph Turner <~ushin/ushin@lists.sr.ht>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a graphviz-based view for `fons' relations data.

;; Some of the code is borrowed from `org-graph-view':
;; https://github.com/alphapapa/org-graph-view, which is GPLv3+ licensed.

;;; Code:

(require 'cl-lib)

(defcustom hyperdrive-fons-view-overlap "voronoi"
  "How to handle overlapping.  See Graphviz documentation.
It seems unclear which is the best default, because each option
renders one or another layout slightly better than other
options."
  :type '(choice (const :tag "Voronoi" "voronoi")
		 (const :tag "Scale" "scale")
		 (const :tag "Allow overlap" "true")
		 (const :tag "VPSC" "vpsc")))

(defcustom hyperdrive-fons-view-layout "dot"
  "Default layout.  See Graphviz documentation."
  :type '(choice
          ((const :description "Pretty good layout.  Offers perspective/root node."
                  "twopi")
           (const :description "Good all-around layout.  No perspective/root node."
                  "neato")
           (const :description "Spacious layout, smaller labels with larger graphs.  Offers perspective/root node."
                  "circo")
           (const :description "Very cozy layout, larger labels.  Some randomness.  No perspective/root node."
                  "fdp")
           (const :description "Top-down, linear layout.  Not very efficient in terms of screen space."
                  "dot")
           (const :description "Similar to fdp." "sfdp"))))

(cl-defun hyperdrive-fons-view
    (paths &key from (layout hyperdrive-fons-view-layout))
  "View RELATION."
  (cl-labels ((window-dimensions-in (&optional (window (selected-window)))
                ;; Return WINDOW (width-in height-in) in inches.
                (with-selected-window window
                  ;; TODO: Ensure we get the monitor the frame is on.
                  (pcase-let* (((map ('geometry `(,_ ,_ ,monitor-width-px ,monitor-height-px))
                                     ('mm-size `(,monitor-width-mm ,monitor-height-mm)))
                                (car (display-monitor-attributes-list)))
                               (monitor-width-in (mm-in monitor-width-mm))
                               (monitor-height-in (mm-in monitor-height-mm))
                               (monitor-width-res (/ monitor-width-px monitor-width-in))
                               (monitor-height-res (/ monitor-height-px monitor-height-in))
                               (window-width-in (/  (window-text-width nil t) monitor-width-res))
                               (window-height-in (/ (window-text-height nil t) monitor-height-res)))
                    (list window-width-in window-height-in
                          monitor-width-res monitor-height-res))))
              (mm-in (mm) (* mm 0.04)))
    (pcase-let* ((`(,width-in ,height-in ,width-res ,height-res)
                  (window-dimensions-in))
                 (graph (flatten-list (hyperdrive-fons-view--paths-graph paths)))
                 (graphviz (hyperdrive-fons-view--format-graph
                            graph :root-name from
                            :layout layout :width-in width-in :height-in height-in
                            ;; Average the two resolutions.
                            :dpi (/ (+ width-res height-res) 2)))
                 (svg-image (hyperdrive-fons-view--svg graphviz))
                 (inhibit-read-only t))
      (with-current-buffer (get-buffer-create "*hyperdrive-fons-view*")
        (erase-buffer)
        (insert-image svg-image)
        (pop-to-buffer (current-buffer))))))

(defun hyperdrive-fons-view--paths-graph (paths)
  "Return graph for PATHS.
Graph is a list of strings which form the graphviz data."
  (let ((nodes (make-hash-table :test #'equal)))
    (cl-labels ((format-path (path)
                  (mapcar #'format-hop (fons-path-hops path)))
                (format-hop (hop)
                  ;; TODO: Hop score.
                  (format "%s -> %s;\n"
                          (fons-hop-from hop) (fons-hop-to hop))))
      (mapcar #'format-path paths))))

(defun hyperdrive-fons-view--relation-graph (relation)
  "Return graph for RELATION.
Graph is a list of strings which form the graphviz data."
  (let ((nodes (make-hash-table :test #'equal)))
    (cl-labels (;; (map-relation (relation)
                ;;   (mapc #'map-path (fons-relation-paths relation)))
                ;; (map-path (path)
                ;;   (mapc #'map-hop (fons-path-hops path)))
                ;; (map-hop (hop)
                ;;   (cl-pushnew hop hops :test #'equal)
                ;;   (cl-pushnew (fons-hop-from hop) nodes :test #'equal)
                ;;   (cl-pushnew (fons-hop-to hop) nodes :test #'equal))
                (format-relation (relation)
                  (mapcar #'format-path (fons-relation-paths relation)))
                (format-path (path)
                  (mapcar #'format-hop (fons-path-hops path)))
                (format-hop (hop)
                  ;; TODO: Hop score.
                  (format "%s -> %s;\n"
                          (fons-hop-from hop) (fons-hop-to hop))))
      (format-relation relation))))

(cl-defun hyperdrive-fons-view--format-graph
    (graph &key root-name width-in height-in layout dpi)
  "Return a graphviz string for GRAPH."
  (cl-labels ((insert-vals (&rest pairs)
                (cl-loop for (key value) on pairs by #'cddr
                         do (insert (format "%s=\"%s\"" key value) "\n")))
              (format-val-list (&rest pairs)
                (s-wrap (s-join "," (cl-loop for (key value) on pairs by #'cddr
                                             collect (format "%s=\"%s\"" key value)))
                        "[" "]")))
    (with-temp-buffer
      (save-excursion
        (insert "digraph fonsrelationview {\n")
        (insert "edge" (format-val-list "color" (face-attribute 'default :foreground)) ";\n")
        (insert "node" (format-val-list "fontname" (face-attribute 'default :family)
				        "nodesep" "1"
				        "mindist" "1")
	        ";\n")
        (insert-vals "layout" layout
                     "bgcolor" (face-attribute 'default :background)
                     "size" (format "%.1d,%.1d" width-in height-in)
                     ;; NOTE: The dpi setting is important, because
                     ;; without it, sometimes cmap areas don't align
                     ;; with the rendered elements.
                     "dpi" (format "%s" dpi)
		     "overlap" hyperdrive-fons-view-overlap
                     "margin" "0"
                     "ratio" "fill"
                     "nodesep" "0"
                     "mindist" "0")
        (mapc #'insert (-flatten graph))
        ;; (maphash (lambda (_key value)
        ;;            (insert (format "%s [%s];\n" (car value)
        ;;                            (s-join ","
        ;;                                    (--map (format "%s=\"%s\"" (car it) (cdr it))
        ;;                                           (node-properties (cdr value)))))))
        ;;          nodes)
        (when root-name
          (insert (format "root=\"%s\"" root-name)))
        (insert "}"))
      ;; (debug-warn (buffer-string))
      (buffer-string)))
  )

(cl-defun hyperdrive-fons-view--svg (graph)
  "Return SVG image for Graphviz GRAPH.
MAP is an Emacs-ready image map to apply to the image's
properties.  SOURCE-BUFFER is the Org buffer the graph displays,
which is applied as a property to the image so map-clicking
commands can find the buffer."
  (with-temp-buffer
    (insert graph)
    (hyperdrive-fons-view--graphviz "svg"
      ;; (debug-warn (buffer-string))
      (save-excursion
	;; HACK: Remove "pt" units from SVG width and height.  See
	;; <https://gitlab.com/graphviz/graphviz/-/issues/867>.
	;; Although it doesn't seem to fix the problem, so some
	;; combinations of window and graph sizes still render parts (or
	;; most) of the SVG off-screen.  *sigh*
	;; (goto-char (point-min))
	;; (when (re-search-forward (rx "<svg width=\"" (group (1+ (not (any "\"")))) "\" "
	;; 			     "height=\"" (group (1+ (not (any "\"")))) "\"")
	;; 			 nil t)
	;;   (replace-match (substring (match-string 1) nil -2) t t nil 1)
	;;   (replace-match (substring (match-string 2) nil -2) t t nil 2))
        )
      (let* ((image (apply #'create-image (buffer-string) 'svg t nil)))
        ;; (setf (image-property image :map) map)
        ;; (setf (image-property image :source-buffer) source-buffer)
        image))))

(defmacro hyperdrive-fons-view--graphviz (type &rest body)
  "Run Graphviz for TYPE on current buffer, then run BODY in it.
Current buffer should contain a Graphviz graph.  Graphviz is
called and replaces the buffer content with the rendered output."
  (declare (indent defun) (debug (stringp body)))
  `(if (zerop (call-process-region (point-min) (point-max) "dot" 'delete t nil
                                   (concat "-T" ,type)))
       (progn
         ,@body)
     (error "Oops: %s" (buffer-string))))

(provide 'hyperdrive-fons-view)
;;; hyperdrive-fons-view.el ends here
