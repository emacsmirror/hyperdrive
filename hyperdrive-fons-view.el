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
(require 'map)

(require 'hyperdrive-fons)

(defgroup hyperdrive-fons-view nil
  "Visualize fons relations."
  :group 'fons)

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

(cl-defun hyperdrive-fons-view
    (hops &key from relations debug
          (layout hyperdrive-fons-view-layout))
  "View HOPS."
  (cl-labels
      ((window-dimensions-in (&optional (window (selected-window)))
         ;; Return WINDOW (width-in height-in) in inches.
         (with-selected-window window
           ;; TODO: Ensure we get the monitor the frame is on.
           (pcase-let* (((map ('geometry
                               `(,_ ,_ ,monitor-width-px ,monitor-height-px))
                              ('mm-size `(,monitor-width-mm ,monitor-height-mm)))
                         (car (display-monitor-attributes-list)))
                        (monitor-width-in (mm-in monitor-width-mm))
                        (monitor-height-in (mm-in monitor-height-mm))
                        (monitor-width-res (/ monitor-width-px monitor-width-in))
                        (monitor-height-res (/ monitor-height-px monitor-height-in))
                        (window-width-in
                         (/  (window-text-width nil t) monitor-width-res))
                        (window-height-in
                         (/ (window-text-height nil t) monitor-height-res)))
             (list window-width-in window-height-in
                   monitor-width-res monitor-height-res))))
       (mm-in (mm) (* mm 0.04)))
    (pcase-let* ((`(,width-in ,height-in ,width-res ,height-res)
                  (window-dimensions-in))
                 (`(,hops-graph ,hops-nodes) (hyperdrive-fons-view--hops-graph hops))
                 (`(,relations-graph ,relations-nodes)
                  (when relations
                    (cl-loop
                     with relations-nodes = (make-hash-table)
                     for (graph nodes)
                     in (mapcar #'hyperdrive-fons-view--relation-graph relations)
                     collect graph into relations-graph
                     and do (setf relations-nodes
                                  (map-merge 'hash-table relations-nodes nodes))
                     finally return (list relations-graph relations-nodes))))
                 (graph (flatten-list (append hops-graph ;; relations-graph
                                              )))
                 (nodes (map-merge 'hash-table hops-nodes relations-nodes))
                 (graphviz-string (hyperdrive-fons-view--format-graph
                                   graph :root-name from :nodes nodes
                                   :layout layout :width-in width-in :height-in height-in
                                   ;; Average the two resolutions.
                                   :dpi (/ (+ width-res height-res) 2)))
                 (svg-image (hyperdrive-fons-view--svg graphviz-string))
                 (inhibit-read-only t))
      (with-current-buffer (get-buffer-create "*hyperdrive-fons-view*")
        (erase-buffer)
        (if debug
            (insert graphviz-string)
          (insert-image svg-image))
        (pop-to-buffer (current-buffer))))))

(defun hyperdrive-fons-view--hops-graph (hops)
  "Return (hops-graph hops-nodes) for HOPS.
Graph is a list of strings which form the graphviz-string data."
  (let ((hops-nodes (make-hash-table :test #'equal)))
    (cl-labels ((format-hop (hop)
                  (setf (gethash hop hops-nodes) (fons-hop-score hop))
                  (format "%s -> %s [label=%s];\n"
                          (fons-hop-from hop) (fons-hop-to hop)
                          (fons-hop-score hop))))
      (list (mapcar #'format-hop hops) hops-nodes))))

(defun hyperdrive-fons-view--paths-graph (paths)
  "Return (hops-graph hops-nodes) for PATHS.
Graph is a list of strings which form the graphviz-string data."
  (let ((hops-nodes (make-hash-table :test #'equal)))
    (cl-labels ((format-path (path)
                  (let ((color (hyperdrive-fons-view--prism-color
                                (concat (fons-hop-from (car (fons-path-hops path)))
                                        (fons-hop-to (car (last (fons-path-hops path)))) ))))
                    (mapcar (lambda (path)
                              (format-hop path color))
                            (fons-path-hops path))))
                (format-hop (hop color)
                  (setf (gethash (fons-hop-to hop) hops-nodes) (fons-hop-score hop))
                  (format "%s -> %s [label=%s color=\"%s\" penwidth=2];\n"
                          (fons-hop-from hop) (fons-hop-to hop)
                          (fons-hop-score hop)
                          color)))
      (list (mapcar #'format-path paths) hops-nodes))))

(defun hyperdrive-fons-view--relation-graph (relation)
  "Return hops-graph for RELATION.
Graph is a list of strings which form the graphviz-string data."
  (let ((nodes (make-hash-table :test #'equal)))
    (cl-labels (;; (map-relation (relation)
                ;;   (mapc #'map-path (fons-relation-paths relation)))
                ;; (map-path (path)
                ;;   (mapc #'map-hop (fons-path-hops path)))
                ;; (map-hop (hop)
                ;;   (cl-pushnew hop hops :test #'equal)
                ;;   (cl-pushnew (fons-hop-from hop) hops-nodes :test #'equal)
                ;;   (cl-pushnew (fons-hop-to hop) hops-nodes :test #'equal))
                (format-whole-relation (relation)
                  (add-node-score relation)
                  (let* ((from (fons-hop-from
                                (car (fons-path-hops
                                      (car (fons-relation-paths relation))))))
                         (to (fons-hop-to
                              (car (last (fons-path-hops
                                          (car (fons-relation-paths relation))))))))
                    (format "%s -> %s [label=%s color=\"%s\" style=\"dotted\"];\n"
                            from to
                            (fons-relation-score relation)
                            (hyperdrive-fons-view--prism-color (concat from to)))))
                (add-node-score (relation)
                  (setf (gethash (fons-relation-to relation) nodes)
                        (fons-relation-score relation)))
                ;; (format-relation (relation)
                ;;   (mapcar #'format-path (fons-relation-paths relation)))
                ;; (format-path (path)
                ;;   (mapcar #'format-hop (fons-path-hops path)))
                ;; (format-hop (hop)
                ;;   (format "%s -> %s [label=%s color=\"%s\" style=\"dotted\"];\n"
                ;;           (fons-hop-from hop) (fons-hop-to hop)
                ;;           (fons-hop-score hop)
                ;;           (hyperdrive-fons-view--prism-color
                ;;            (concat (fons-hop-from hop) (fons-hop-to hop)))))
                )
      (list (format-whole-relation relation) nodes))))

(cl-defun hyperdrive-fons-view--format-graph
    (hops-graph &key nodes root-name width-in height-in layout dpi)
  "Return a graphviz-string string for GRAPH."
  (cl-labels ((insert-vals (&rest pairs)
                (cl-loop for (key value) on pairs by #'cddr
                         do (insert (format "%s=\"%s\"" key value) "\n")))
              (format-val-list (&rest pairs)
                (s-wrap (string-join (cl-loop for (key value) on pairs by #'cddr
                                              collect (format "%s=\"%s\"" key value))
                                     ",")
                        "[" "]"))
              (format-node-label (key value)
                (let ((name (cl-typecase key
                              (string key)
                              (fons-hop (fons-hop-to key))))
                      (value (format "%.2f" value)))
                  (insert (format "%s [label=\"%s (%s)\"];\n" name name value)))))
    ;; (setf width-in (/ width-in 1.5)
    ;;       height-in (/ height-in 1.5))
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
        (mapc #'insert (flatten-list hops-graph))
        (maphash #'format-node-label nodes)
        (when root-name
          (insert (format "root=\"%s\"" root-name)))
        (insert "}"))
      ;; (debug-warn (buffer-string))
      (buffer-string))))

(cl-defun hyperdrive-fons-view--svg (hops-graph)
  "Return SVG image for Graphviz GRAPH.
MAP is an Emacs-ready image map to apply to the image's
properties.  SOURCE-BUFFER is the Org buffer the hops-graph displays,
which is applied as a property to the image so map-clicking
commands can find the buffer."
  (with-temp-buffer
    (insert hops-graph)
    (hyperdrive-fons-view--graphviz "svg"
      ;; (debug-warn (buffer-string))
      (save-excursion
	;; HACK: Remove "pt" units from SVG width and height.  See
	;; <https://gitlab.com/graphviz/graphviz/-/issues/867>.
	;; Although it doesn't seem to fix the problem, so some
	;; combinations of window and hops-graph sizes still render parts (or
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

(defvar hyperdrive-fons-view-prism-minimum-contrast 6
  "Attempt to enforce this minimum contrast ratio for user faces.
This should be a reasonable number from, e.g. 0-7 or so."
  ;; Prot would almost approve of this default.  :) I would go all the way
  ;; to 7, but 6 already significantly dilutes the colors in some cases.
  )

(cl-defun hyperdrive-fons-view--prism-color
    (string &key (contrast-with (face-background 'default nil 'default)))
  ;; Copied from ement.el.
  "Return a computed color for STRING.
The color is adjusted to have sufficient contrast with the color
CONTRAST-WITH (by default, the default face's background).  The
computed color is useful for user messages, generated room
avatars, etc."
  (require 'color)
  ;; TODO: Use this instead of `ement-room--user-color'.  (Same algorithm ,just takes a
  ;; string as argument.)
  ;; TODO: Try using HSV somehow so we could avoid having so many strings return a
  ;; nearly-black color.
  (cl-labels ((relative-luminance (rgb)
                ;; Copy of `modus-themes-wcag-formula', an elegant
                ;; implementation by Protesilaos Stavrou.  Also see
                ;; <https://en.wikipedia.org/wiki/Relative_luminance> and
                ;; <https://www.w3.org/TR/WCAG20/#relativeluminancedef>.
                (cl-loop for k in '(0.2126 0.7152 0.0722)
                         for x in rgb
                         sum (* k (if (<= x 0.03928)
                                      (/ x 12.92)
                                    (expt (/ (+ x 0.055) 1.055) 2.4)))))
              (contrast-ratio (a b)
                ;; Copy of `modus-themes-contrast'; see above.
                (let ((ct (/ (+ (relative-luminance a) 0.05)
                             (+ (relative-luminance b) 0.05))))
                  (max ct (/ ct))))
              (increase-contrast (color against target toward)
                (let ((gradient (cdr (color-gradient color toward 20)))
                      new-color)
                  (cl-loop do (setf new-color (pop gradient))
                           while new-color
                           until (>= (contrast-ratio new-color against) target)
                           ;; Avoid infinite loop in case of weirdness
                           ;; by returning color as a fallback.
                           finally return (or new-color color)))))
    (let* ((id string)
           (id-hash (float (abs (sxhash id))))
           ;; TODO: Wrap-around the value to get the color I want.
           (ratio (/ id-hash (float most-positive-fixnum)))
           (color-num (round (* (* 255 255 255) ratio)))
           (color-rgb (list (/ (float (logand color-num 255)) 255)
                            (/ (float (ash (logand color-num 65280) -8)) 255)
                            (/ (float (ash (logand color-num 16711680) -16)) 255)))
           (contrast-with-rgb (color-name-to-rgb contrast-with)))
      (when (< (contrast-ratio color-rgb contrast-with-rgb) hyperdrive-fons-view-prism-minimum-contrast)
        (setf color-rgb (increase-contrast color-rgb contrast-with-rgb hyperdrive-fons-view-prism-minimum-contrast
                                           (color-name-to-rgb
                                            ;; Ideally we would use the foreground color,
                                            ;; but in some themes, like Solarized Dark,
                                            ;; the foreground color's contrast is too low
                                            ;; to be effective as the value to increase
                                            ;; contrast against, so we use white or black.
                                            (pcase contrast-with
                                              ((or `nil "unspecified-bg")
                                               ;; The `contrast-with' color (i.e. the
                                               ;; default background color) is nil.  This
                                               ;; probably means that we're displaying on
                                               ;; a TTY.
                                               (if (fboundp 'frame--current-backround-mode)
                                                   ;; This function can tell us whether
                                                   ;; the background color is dark or
                                                   ;; light, but it was added in Emacs
                                                   ;; 28.1.
                                                   (pcase (frame--current-backround-mode (selected-frame))
                                                     ('dark "white")
                                                     ('light "black"))
                                                 ;; Pre-28.1: Since faces' colors may be
                                                 ;; "unspecified" on TTY frames, in which
                                                 ;; case we have nothing to compare with, we
                                                 ;; assume that the background color of such
                                                 ;; a frame is black and increase contrast
                                                 ;; toward white.
                                                 "white"))
                                              (_
                                               ;; The `contrast-with` color is usable: test it.
                                               (if (color-dark-p (color-name-to-rgb contrast-with))
                                                   "white" "black")))))))
      (apply #'color-rgb-to-hex (append color-rgb (list 2))))))

(provide 'hyperdrive-fons-view)
;;; hyperdrive-fons-view.el ends here
