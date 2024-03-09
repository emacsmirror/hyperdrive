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

;;;; Requirements

(require 'cl-lib)
(require 'map)

(require 'hyperdrive-fons)

;;;; Customization

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

(defcustom hyperdrive-fons-view-source-color "green"
  "Source edge and node color.
May be any string listed here:
<https://graphviz.org/doc/info/colors.html>."
  ;; Don't use :type 'color.  Emacs colors don't necessarily match graphviz
  ;; colors, and they may contain spaces, preventing the graph from rendering.
  :type 'string)

;;;; Macros

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

;;;; Functions

(cl-defun hyperdrive-fons-view
    (relations from &key debug (layout hyperdrive-fons-view-layout))
  "View RELATIONS from FROM."
  (pcase-let* ((hops
                (hyperdrive-fons-view--relations-graph relations))
               (graphviz-string
                (hyperdrive-fons-view--format-graph
                 hops relations :root-name from :layout layout)))
    (hyperdrive-fons-view--render-graphviz graphviz-string)))

(cl-defun hyperdrive-fons-view--render-graphviz (graphviz &key buffer)
  "Render GRAPHVIZ string in BUFFER scaled by SCALE."
  (with-current-buffer (get-buffer-create (or buffer "*hyperdrive-fons-view*"))
    (let* ((image-map (hyperdrive-fons-view--graph-map graphviz))
           (svg-string (hyperdrive-fons-view--svg graphviz))
           (image (create-image svg-string 'svg t :map image-map))
           (inhibit-read-only t))
      (when (> 30 emacs-major-version)
        ;; In Emacs 30, :unscaled-map is automatically added (bug#69602)
        (setq image (nconc image
                           (list :unscaled-map (copy-tree image-map t)))))
      (erase-buffer)
      (insert-image image)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun hyperdrive-fons-view--image-scale-map (map factor)
  "Copy of `image--scale-map', added in Emacs 30.  Accepts MAP, FACTOR."
  (unless (= 1 factor)
    (pcase-dolist (`(,`(,type . ,coords) ,_id ,_plist) map)
      (pcase-exhaustive type
        ('rect
         (setf (caar coords) (round (* (caar coords) factor)))
         (setf (cdar coords) (round (* (cdar coords) factor)))
         (setf (cadr coords) (round (* (cadr coords) factor)))
         (setf (cddr coords) (round (* (cddr coords) factor))))
        ('circle
         (setf (caar coords) (round (* (caar coords) factor)))
         (setf (cdar coords) (round (* (cdar coords) factor)))
         (setf (cdr coords) (round (* (cdr coords) factor))))
        ('poly
         (dotimes (i (length coords))
           (aset coords i
                 (round (* (aref coords i) factor))))))))
  map)

(advice-add #'image--change-size :after #'image--change-size-scale-map)

(defun image--change-size-scale-map (_factor &optional position)
  "Scale :map property of image at point to fit its :scale.
Intended to be used as :after advice for `image--change-size'."
  (when-let* ((image (image--get-imagemagick-and-warn position))
              (map (image-property image :map))
              (unscaled-map (image-property image :unscaled-map))
              (scale (image-property image :scale)))
    (setf (image-property image :map)
          (hyperdrive-fons-view--image-scale-map (copy-tree unscaled-map t) scale))))

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

(defun hyperdrive-fons-view--graph-map (graph)
  "Return image map for Graphviz GRAPH."
  ;; TODO: Remove dash.el and s.el dependencies
  (with-temp-buffer
    (insert graph)
    (hyperdrive-fons-view--graphviz "cmapx"
      ;; (debug-warn (buffer-string))
      (cl-labels ((convert-map
		    (map) (-let (((_map _props . areas) map))
			    (mapcar #'convert-area areas)))
                  (convert-area
		    (area) (-let (((_area (&alist 'shape 'title 'href 'coords)) area))
			     (list (pcase-exhaustive shape
				     ("circle" (cons 'circle (convert-circle coords)))
				     ("poly" (cons 'poly (convert-poly coords)))
				     ("rect" (cons 'rect (convert-rect coords))))
				   href (list 'help-echo title))))
                  (convert-circle
		    (coords) (-let (((x y r) (->> coords (s-split ",") (-map #'string-to-number))))
			       (cons (cons x y) r)))
                  (convert-poly
		    (coords) (->> coords (s-split ",") (-map #'string-to-number) (apply #'vector)))
                  (convert-rect
		    (coords) (-let (((x0 y0 x1 y1)
				     (->> coords (s-split ",") (-map #'string-to-number))))
			       (cons (cons x0 y0) (cons x1 y1)))))
        (let* ((cmapx (libxml-parse-xml-region (point-min) (point-max))))
          (convert-map cmapx))))))

(defun hyperdrive-fons-view--relations-graph (relations)
  "Return hops for RELATIONS.
RELATIONS may be list of `fons-relation' structs."
  (let (hops)
    (cl-labels ((map-relation (to relation)
                  (mapc #'map-path (fons-relation-paths relation)))
                (map-path (path)
                  (mapc #'map-hop (fons-path-hops path)))
                (map-hop (hop)
                  ;; TODO: Benchmark alternative methods to collect hops:
                  ;; 1. push, then delete-dups
                  ;; 2. (unless (cl-find hop hops) (push hop hops))
                  ;; 3. ???
                  (cl-pushnew hop hops :test #'equal)))
      (maphash #'map-relation relations)
      hops)))

(defun hyperdrive-fons-view--format-hop (hop color)
  "Return graphviz-string for HOP."
  (format "%s -> %s [label=%s color=\"%s\" penwidth=2];\n"
          (fons-hop-from hop) (fons-hop-to hop)
          (fons-hop-score hop) color))

(cl-defun hyperdrive-fons-view--format-graph (hops relations &key root-name layout)
  "Return a graphviz-string string for HOPS."
  (cl-labels ((insert-vals (&rest pairs)
                (cl-loop for (key value) on pairs by #'cddr
                         do (insert (format "%s=\"%s\"" key value) "\n")))
              (format-val-list (&rest pairs)
                (s-wrap (string-join (cl-loop for (key value) on pairs by #'cddr
                                              collect (format "%s=\"%s\"" key value))
                                     ",")
                        "[" "]"))
              (format-relation-label (to relation)
                (let ((score (format "%.2f" (fons-relation-score relation))))
                  (insert
                   (format
                    "%s [label=\"%s (%s)\", href=\"%s\", shape=\"ellipse\", color=\"%s\"];\n"
                    to to score to hyperdrive-fons-view-source-color)))))
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
                     "overlap" hyperdrive-fons-view-overlap
                     "margin" "0"
                     "ratio" "fill"
                     "nodesep" "0"
                     "mindist" "0")
        (mapc #'insert (mapcar (lambda (hop)
                                 (hyperdrive-fons-view--format-hop
                                  hop hyperdrive-fons-view-source-color))
                               hops))
        (maphash #'format-relation-label relations)
        (when root-name
          (insert (format "root=\"%s\"" root-name)))
        (insert "}"))
      ;; (debug-warn (buffer-string))
      (buffer-string))))

(cl-defun hyperdrive-fons-view--svg (hops-graph)
  "Return SVG string for Graphviz GRAPH."
  (with-temp-buffer
    (insert hops-graph)
    (hyperdrive-fons-view--graphviz "svg"
      (buffer-string))))

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

;; TODO: Bookmark support

(provide 'hyperdrive-fons-view)
;;; hyperdrive-fons-view.el ends here
