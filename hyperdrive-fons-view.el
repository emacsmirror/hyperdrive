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

;; Some of the code is copied from `org-graph-view':
;; https://github.com/alphapapa/org-graph-view, which is GPLv3+ licensed.

;;; Code:

;; TODO: Bookmark support

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'color)

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

(defun hyperdrive-fons-view-blend-with (name alpha attr)
  "Return hex of color NAME blended with attr with ALPHA.
TYPE may be a face attribute, like `:foreground' or
 `:background'."
  (apply #'color-rgb-to-hex
         `(,@(hyperdrive-fons-view-blend
              (color-name-to-rgb name)
              (color-name-to-rgb (face-attribute 'default attr))
              alpha)
           2)))

(defun hyperdrive-fons-view-blend (a b &optional alpha)
  "Blend the two colors A and B with ALPHA.
A and B should be lists (RED GREEN BLUE), where each element is
between 0.0 and 1.0, inclusive.  ALPHA controls the influence A
has on the result and should be between 0.0 and 1.0, inclusive."
  (setq alpha (or alpha 0.5))
  (let (blend)
    (dotimes (i 3)
      (push (+ (* (nth i a) alpha) (* (nth i b) (- 1 alpha))) blend))
    (nreverse blend)))

;; TODO: Reload image on defcustom change
(defcustom hyperdrive-fons-view-sources-color "green"
  "Source edge and node color.
May be any string listed here:
<https://graphviz.org/doc/info/colors.html>."
  ;; Don't use :type 'color.  Emacs colors don't necessarily match graphviz
  ;; colors, and they may contain spaces, preventing the graph from rendering.
  :type 'string)

(defcustom hyperdrive-fons-view-blockers-color "blue"
  "Blocker edge and node color.
May be any string listed here:
<https://graphviz.org/doc/info/colors.html>."
  ;; Don't use :type 'color.  Emacs colors don't necessarily match graphviz
  ;; colors, and they may contain spaces, preventing the graph from rendering.
  :type 'string)

(defcustom hyperdrive-fons-view-blocked-color "red"
  "Blocked edge and node color.
May be any string listed here:
<https://graphviz.org/doc/info/colors.html>."
  ;; Don't use :type 'color.  Emacs colors don't necessarily match graphviz
  ;; colors, and they may contain spaces, preventing the graph from rendering.
  :type 'string)

;;;; Mode

(define-derived-mode hyperdrive-fons-view-mode special-mode
  `("Hyperdrive-fons-view"
    ;; TODO: Add more to lighter, e.g. menu to change params.
    )
  "Major mode for viewing Hyperdrive Fons graphs."
  :group 'hyperdrive
  :interactive nil
  ;; `pixel-scroll-precision-mode' makes <wheel-up>/<wheel-down> not scroll.
  ;; TODO: Test and report potential Emacs bug with scrolling images with
  ;; `pixel-scroll-precision-mode'.
  ;; (pixel-scroll-precision-mode -1)
  (setq-local cursor-type nil)
  ;; TODO: Consider effect of `fast-but-imprecise-scrolling'.

  ;; Required by `pixel-scroll-precision-mode' bug#66769 and bug#68196
  ;; TODO: In order to use `pixel-scroll-precision-mode':
  ;; (setq-local scroll-margin 0)
  ;; TODO: Prevent scrolling past right edge of image.  Similar issue: #68786

  ;; TODO: For images jumping when switching windows, see bug#70038:
  ;; <https://yhetil.org/emacs-bugs/871q7qckd7.fsf@uam.es/T/#u> and
  ;; https://github.com/vedang/pdf-tools/pull/224
  (setq-local mouse-wheel-tilt-scroll t))
;; FIXME: Prevent image from scrolling off the bottom of the screen.

;;;; Functions

(cl-defun hyperdrive-fons-view--window-dimensions (&optional (window (selected-window)))
  "Return (WIDTH HEIGHT DPI) for WINDOW.
WIDTH and HEIGHT are in inches."
  (with-selected-window window
    (pcase-let* (((map ('geometry `(,_ ,_ ,monitor-width-px ,monitor-height-px))
                       ('mm-size `(,monitor-width-mm ,monitor-height-mm)))
                  (car (display-monitor-attributes-list)))
                 (width-in (* 0.04 monitor-width-mm))
                 (height-in (* 0.04 monitor-height-mm))
                 (width-res (/ monitor-width-px width-in))
                 (height-res (/ monitor-height-px height-in))
                 (width-in (/  (window-text-width nil t) width-res))
                 (height-in (/ (window-text-height nil t) height-res)))
      (list width-in height-in (/ (+ width-res height-res) 2)))))

(cl-defun hyperdrive-fons-view
    (relations root &key topics (layout hyperdrive-fons-view-layout) focus-ids label-fun)
  "View RELATIONS from ROOT."
  (hyperdrive-fons-view--render-graphviz
   (hyperdrive-fons-view--format-graph
    relations :root-name root :topics topics :focus-ids focus-ids
    :layout layout :label-fun label-fun)))

(defun hyperdrive-fons-view--graphviz (type)
  "Run Graphviz for TYPE on current buffer.
Graphviz is called on current buffer content, which should be a
graphviz string, and replaces it with the rendered output."
  (unless (zerop (call-process-region (point-min) (point-max) "dot" 'delete t nil
                                      (concat "-T" type)))
    (error "Error generating graph: %S" (buffer-string))))

(cl-defun hyperdrive-fons-view--render-graphviz (graphviz)
  "Render GRAPHVIZ string in current buffer."
  (let* ((original-map (hyperdrive-fons-view--graph-map graphviz))
         (svg-string (hyperdrive-fons-view--svg graphviz))
         (inhibit-read-only t)
         (image (create-image svg-string 'svg t :original-map original-map)))
    (when (> 30 emacs-major-version)
      ;; TODO(deprecate-29): (bug#69602) resolved in Emacs 30.
      (setq image (nconc image (list :map (copy-tree original-map t)))))
    (erase-buffer)
    (insert-image image)
    (goto-char (point-min))))

(defun hyperdrive-fons-view--graph-map (graph)
  "Return image map for Graphviz GRAPH."
  (with-temp-buffer
    (insert graph)
    (hyperdrive-fons-view--graphviz "cmapx")
    (mapcar (lambda (area)
              (pcase-let* ((`(area ,(map shape href coords)) area)
                           (coords-list (mapcar #'string-to-number
                                                (split-string coords ","))))
                (list (pcase-exhaustive shape
        	        ("circle" (pcase-let ((`(,x ,y ,r) coords-list))
                                    (cons 'circle (cons (cons x y) r))))
        	        ("poly" (cons 'poly (vconcat coords-list)))
        	        ("rect" (pcase-let ((`(,x0 ,y0 ,x1 ,y1) coords-list))
                                  (cons 'rect
                                        (cons (cons x0 y0) (cons x1 y1))))))
        	      href (list 'help-echo href))))
            (cddr (libxml-parse-xml-region (point-min) (point-max))))))

(cl-defun hyperdrive-fons-view--format-graph
    (relations &key root-name topics focus-ids layout (label-fun #'identity))
  "Return a graphviz-string string for RELATIONS."
  (let ((sources-node-color (hyperdrive-fons-view-blend-with
                             hyperdrive-fons-view-sources-color 0.25 :background))
        (sources-edge-color (hyperdrive-fons-view-blend-with
                             hyperdrive-fons-view-sources-color 0.5 :foreground))
        (blockers-node-color (hyperdrive-fons-view-blend-with
                              hyperdrive-fons-view-blockers-color 0.25 :background))
        (blockers-edge-color (hyperdrive-fons-view-blend-with
                              hyperdrive-fons-view-blockers-color 0.5 :foreground))
        (blocked-node-color (hyperdrive-fons-view-blend-with
                             hyperdrive-fons-view-blocked-color 0.25 :background))
        (blocked-edge-color (hyperdrive-fons-view-blend-with
                             hyperdrive-fons-view-blocked-color 0.5 :foreground)))
    (cl-labels ((insert-vals (&rest pairs)
                  (cl-loop for (key value) on pairs by #'cddr
                           do (insert (format "%s=\"%s\"" key value) "\n")))
                (format-val-list (&rest pairs)
                  (format "[%s]" (string-join
                                  (cl-loop for (key value) on pairs by #'cddr
                                           collect (format "%s=\"%s\"" key value))
                                  ",")))
                (format-hop (hop color)
                  (format "%s -> %s [color=\"%s\" penwidth=2];\n"
                          (fons-hop-from hop) (fons-hop-to hop) color))
                (format-to (to relation)
                  (insert
                   (format
                    "%s [label=%s, href=\"%s\", shape=\"%s\", color=\"%s\", style=\"filled\", penwidth=\"4\", fillcolor=\"%s;0.5:%s\"];\n"
                    to (funcall label-fun to) to
                    (if (member to focus-ids)
                        "house"
                      "ellipse")
                    "grey"
                    ;; Prioritize blocked > source > blocker
                    (cond
                     ((fons-relation-blocked-paths relation) blocked-node-color)
                     ((fons-relation-source-paths relation) sources-node-color)
                     ((fons-relation-blocker-paths relation) blockers-node-color))
                    ;; Prioritize blocker > blocked > source
                    (cond
                     ((fons-relation-blocker-paths relation) blockers-node-color)
                     ((fons-relation-blocked-paths relation) blocked-node-color)
                     ((fons-relation-source-paths relation) sources-node-color)))))
                (format-root (root)
                  (insert (format
                           "%s [label=%s, href=\"%s\", shape=\"invhouse\", penwidth=\"4\", color=\"%s\", style=\"filled\", fillcolor=\"%s;0.5:%s\"];\n"
                           root (funcall label-fun root) root "grey"
                           sources-node-color blockers-node-color))
                  (insert (format "{ rank=\"source\"; %s; }\n" root))
                  (insert (format "root=\"%s\"\n" root-name))))
      (with-temp-buffer
        (save-excursion
          (insert "digraph fonsrelationview {\n")
          (insert "edge" (format-val-list "color" (face-attribute 'default :foreground)) ";\n")
          (insert "node" (format-val-list "fontname" (face-attribute 'default :family)
				          "mindist" "1")
	          ";\n")
          (insert-vals "layout" layout
                       "bgcolor" (face-attribute 'default :background)
                       "overlap" hyperdrive-fons-view-overlap
                       "nodesep""1"
                       "ranksep""1"
                       "margin" "0"
                       "ratio" "fill"
                       "mindist" "0")
          (dolist (topic topics)
            ;; TODO: Group nodes and edges by topic
            (dolist (hop (fons-relations-hops relations 'sources topic))
              (insert (format-hop hop sources-edge-color))))
          (dolist (hop (fons-relations-hops relations 'blockers))
            (insert (format-hop hop blockers-edge-color)))
          (when (catch 'blocker-paths-exist-p
                  ;; Only display block hops when blockers are displayed.
                  (maphash (lambda (_id relation)
                             (when (fons-relation-blocker-paths relation)
                               (throw 'blocker-paths-exist-p t)))
                           relations))
            (dolist (hop (fons-relations-hops relations 'blocked))
              (insert (format-hop hop blocked-edge-color))))
          (format-root root-name)
          (maphash #'format-to relations)
          (insert "}"))
        (buffer-string)))))

(cl-defun hyperdrive-fons-view--svg (graph)
  "Return SVG string for Graphviz GRAPH."
  (with-temp-buffer
    (insert graph)
    (hyperdrive-fons-view--graphviz "svg")
    (buffer-string)))

;; (defvar hyperdrive-fons-view-prism-minimum-contrast 6
;;   "Attempt to enforce this minimum contrast ratio for user faces.
;; This should be a reasonable number from, e.g. 0-7 or so."
;;   ;; Prot would almost approve of this default.  :) I would go all the way
;;   ;; to 7, but 6 already significantly dilutes the colors in some cases.
;;   )

;; (cl-defun hyperdrive-fons-view--prism-color
;;     (string &key (contrast-with (face-background 'default nil 'default)))
;;   ;; Copied from ement.el.
;;   "Return a computed color for STRING.
;; The color is adjusted to have sufficient contrast with the color
;; CONTRAST-WITH (by default, the default face's background).  The
;; computed color is useful for user messages, generated room
;; avatars, etc."
;;   (require 'color)
;;   ;; TODO: Use this instead of `ement-room--user-color'.  (Same algorithm ,just takes a
;;   ;; string as argument.)
;;   ;; TODO: Try using HSV somehow so we could avoid having so many strings return a
;;   ;; nearly-black color.
;;   (cl-labels ((relative-luminance (rgb)
;;                 ;; Copy of `modus-themes-wcag-formula', an elegant
;;                 ;; implementation by Protesilaos Stavrou.  Also see
;;                 ;; <https://en.wikipedia.org/wiki/Relative_luminance> and
;;                 ;; <https://www.w3.org/TR/WCAG20/#relativeluminancedef>.
;;                 (cl-loop for k in '(0.2126 0.7152 0.0722)
;;                          for x in rgb
;;                          sum (* k (if (<= x 0.03928)
;;                                       (/ x 12.92)
;;                                     (expt (/ (+ x 0.055) 1.055) 2.4)))))
;;               (contrast-ratio (a b)
;;                 ;; Copy of `modus-themes-contrast'; see above.
;;                 (let ((ct (/ (+ (relative-luminance a) 0.05)
;;                              (+ (relative-luminance b) 0.05))))
;;                   (max ct (/ ct))))
;;               (increase-contrast (color against target toward)
;;                 (let ((gradient (cdr (color-gradient color toward 20)))
;;                       new-color)
;;                   (cl-loop do (setf new-color (pop gradient))
;;                            while new-color
;;                            until (>= (contrast-ratio new-color against) target)
;;                            ;; Avoid infinite loop in case of weirdness
;;                            ;; by returning color as a fallback.
;;                            finally return (or new-color color)))))
;;     (let* ((id string)
;;            (id-hash (float (abs (sxhash id))))
;;            ;; TODO: Wrap-around the value to get the color I want.
;;            (ratio (/ id-hash (float most-positive-fixnum)))
;;            (color-num (round (* (* 255 255 255) ratio)))
;;            (color-rgb (list (/ (float (logand color-num 255)) 255)
;;                             (/ (float (ash (logand color-num 65280) -8)) 255)
;;                             (/ (float (ash (logand color-num 16711680) -16)) 255)))
;;            (contrast-with-rgb (color-name-to-rgb contrast-with)))
;;       (when (< (contrast-ratio color-rgb contrast-with-rgb) hyperdrive-fons-view-prism-minimum-contrast)
;;         (setf color-rgb (increase-contrast color-rgb contrast-with-rgb hyperdrive-fons-view-prism-minimum-contrast
;;                                            (color-name-to-rgb
;;                                             ;; Ideally we would use the foreground color,
;;                                             ;; but in some themes, like Solarized Dark,
;;                                             ;; the foreground color's contrast is too low
;;                                             ;; to be effective as the value to increase
;;                                             ;; contrast against, so we use white or black.
;;                                             (pcase contrast-with
;;                                               ((or `nil "unspecified-bg")
;;                                                ;; The `contrast-with' color (i.e. the
;;                                                ;; default background color) is nil.  This
;;                                                ;; probably means that we're displaying on
;;                                                ;; a TTY.
;;                                                (if (fboundp 'frame--current-backround-mode)
;;                                                    ;; This function can tell us whether
;;                                                    ;; the background color is dark or
;;                                                    ;; light, but it was added in Emacs
;;                                                    ;; 28.1.
;;                                                    (pcase (frame--current-backround-mode (selected-frame))
;;                                                      ('dark "white")
;;                                                      ('light "black"))
;;                                                  ;; Pre-28.1: Since faces' colors may be
;;                                                  ;; "unspecified" on TTY frames, in which
;;                                                  ;; case we have nothing to compare with, we
;;                                                  ;; assume that the background color of such
;;                                                  ;; a frame is black and increase contrast
;;                                                  ;; toward white.
;;                                                  "white"))
;;                                               (_
;;                                                ;; The `contrast-with` color is usable: test it.
;;                                                (if (color-dark-p (color-name-to-rgb contrast-with))
;;                                                    "white" "black")))))))
;;       (apply #'color-rgb-to-hex (append color-rgb (list 2))))))

;;;;; Compatibility with pre-Emacs 30 image.el

;; TODO(deprecate-29): (bug#69602) resolved in Emacs 30.
;; In Emacs 30, image maps are transformed along with the image (bug#69602).

(when (> 30 emacs-major-version)
  ;; Adding this global :after advice should not interfere with other packages
  ;; since it has no effect on images that lack an :original-map property.
  (advice-add #'image--change-size
              :after #'hyperdrive-fons-view--recompute-image-map-at-point)
  (advice-add #'image-rotate
              :after #'hyperdrive-fons-view--recompute-image-map-at-point)
  (advice-add #'image-flip-horizontally
              :after #'hyperdrive-fons-view--recompute-image-map-at-point)
  (advice-add #'image-flip-vertically
              :after #'hyperdrive-fons-view--recompute-image-map-at-point))

(defun hyperdrive-fons-view--recompute-image-map-at-point (&rest _args)
  "Recompute :map for image at point.
Intended as :after advice for commands which transform images."
  (when-let* ((image (image--get-image))
              (original-map (image-property image :original-map)))
    (setf (image-property image :map)
          (hyperdrive-fons-view-image--compute-map image))))

(defun hyperdrive-fons-view-image--compute-map (image)
  "Compute map for IMAGE suitable to be used as its :map property.
Return a copy of :original-image transformed based on IMAGE's :scale,
:rotation, and :flip.  When IMAGE's :original-map is nil, return nil.
When :rotation is not a multiple of 90, return copy of :original-map."
  (pcase-let* ((original-map (image-property image :original-map))
               (map (copy-tree original-map t))
               (scale (or (image-property image :scale) 1))
               (rotation (or (image-property image :rotation) 0))
               (flip (image-property image :flip))
               ((and size `(,width . ,height)) (image-size image t)))
    (when (and ; Handle only 90-degree rotations
           (zerop (mod rotation 1))
           (zerop (% (truncate rotation) 90)))
      ;; SIZE fits MAP after transformations.  Scale MAP before
      ;; flip and rotate operations, since both need MAP to fit SIZE.
      (hyperdrive-fons-view-image--scale-map map scale)
      ;; In rendered images, rotation is always applied before flip.
      (hyperdrive-fons-view-image--rotate-map
       map rotation (if (or (= 90 rotation) (= 270 rotation))
                        ;; If rotated ±90°, swap width and height.
                        (cons height width)
                      size))
      ;; After rotation, there's no need to swap width and height.
      (hyperdrive-fons-view-image--flip-map map flip size))
    map))

;; `hyperdrive-fons-view-image--compute-original-map' will be necessary if we
;; want to be able to transform images before they are first displayed.

;; (defun hyperdrive-fons-view-image--compute-original-map (image)
;;   "Return original map for IMAGE.
;; If IMAGE lacks :map property, return nil.
;; When :rotation is not a multiple of 90, return copy of :map."
;;   (when (image-property image :map)
;;     (let* ((original-map (copy-tree (image-property image :map) t))
;;            (scale (or (image-property image :scale) 1))
;;            (rotation (or (image-property image :rotation) 0))
;;            (flip (image-property image :flip))
;;            (size (image-size image t)))
;;       (when (and ; Handle only 90-degree rotations
;;              (zerop (mod rotation 1))
;;              (zerop (% (truncate rotation) 90)))
;;         ;; In rendered images, rotation is always applied before flip.
;;         ;; To undo the transformation, flip before rotating.  SIZE fits
;;         ;; ORIGINAL-MAP before transformations are applied.  Therefore,
;;         ;; scale ORIGINAL-MAP after flip and rotate operations, since
;;         ;; both need ORIGINAL-MAP to fit SIZE.
;;         (hyperdrive-fons-view-image--flip-map map flip size)
;;         (hyperdrive-fons-view-image--rotate-map map (- rotation) size)
;;         (hyperdrive-fons-view-image--scale-map map (/ 1.0 scale)))
;;       original-map)))

(defun hyperdrive-fons-view-image--scale-map (map scale)
  "Scale MAP according to SCALE.
Destructively modifies and returns MAP."
  (unless (= 1 scale)
    (pcase-dolist (`(,`(,type . ,coords) ,_id ,_plist) map)
      (pcase-exhaustive type
        ('rect
         (setf (caar coords) (round (* (caar coords) scale)))
         (setf (cdar coords) (round (* (cdar coords) scale)))
         (setf (cadr coords) (round (* (cadr coords) scale)))
         (setf (cddr coords) (round (* (cddr coords) scale))))
        ('circle
         (setf (caar coords) (round (* (caar coords) scale)))
         (setf (cdar coords) (round (* (cdar coords) scale)))
         (setcdr coords (round (* (cdr coords) scale))))
        ('poly
         (dotimes (i (length coords))
           (aset coords i
                 (round (* (aref coords i) scale))))))))
  map)

(defun hyperdrive-fons-view-image--rotate-map (map rotation size)
  "Rotate MAP according to ROTATION and SIZE.
Destructively modifies and returns MAP."
  (unless (zerop rotation)
    (pcase-dolist (`(,`(,type . ,coords) ,_id ,_plist) map)
      (pcase-exhaustive type
        ('rect
         (let ( x0 y0  ; New upper left corner
                x1 y1) ; New bottom right corner
           (pcase (truncate (mod rotation 360)) ; Set new corners to...
             (90 ; ...old bottom left and upper right
              (setq x0 (caar coords) y0 (cddr coords)
                    x1 (cadr coords) y1 (cdar coords)))
             (180 ; ...old bottom right and upper left
              (setq x0 (cadr coords) y0 (cddr coords)
                    x1 (caar coords) y1 (cdar coords)))
             (270 ; ...old upper right and bottom left
              (setq x0 (cadr coords) y0 (cdar coords)
                    x1 (caar coords) y1 (cddr coords))))
           (setcar coords (hyperdrive-fons-view-image--rotate-coord x0 y0 rotation size))
           (setcdr coords (hyperdrive-fons-view-image--rotate-coord x1 y1 rotation size))))
        ('circle
         (setcar coords (hyperdrive-fons-view-image--rotate-coord
                         (caar coords) (cdar coords) rotation size)))
        ('poly
         (dotimes (i (length coords))
           (when (= 0 (% i 2))
             (pcase-let ((`(,x . ,y)
                          (hyperdrive-fons-view-image--rotate-coord
                           (aref coords i) (aref coords (1+ i)) rotation size)))
               (aset coords i x)
               (aset coords (1+ i) y))))))))
  map)

(defun hyperdrive-fons-view-image--rotate-coord (x y angle size)
  "Rotate coordinates X and Y by ANGLE in image of SIZE.
ANGLE must be a multiple of 90.  Returns a cons cell of rounded
coordinates (X1 Y1)."
  (pcase-let* ((radian (* (/ angle 180.0) float-pi))
               (`(,width . ,height) size)
               ;; y is positive, but we are in the bottom-right quadrant
               (y (- y))
               ;; Rotate clockwise
               (x1 (+ (* (sin radian) y) (* (cos radian) x)))
               (y1 (- (* (cos radian) y) (* (sin radian) x)))
               ;; Translate image back into bottom-right quadrant
               (`(,x1 . ,y1)
                (pcase (truncate (mod angle 360))
                  (90 ; Translate right by height
                   (cons (+ x1 height) y1))
                  (180 ; Translate right by width and down by height
                   (cons (+ x1 width) (- y1 height)))
                  (270 ; Translate down by width
                   (cons x1 (- y1 width)))))
               ;; Invert y1 to make both x1 and y1 positive
               (y1 (- y1)))
    (cons (round x1) (round y1))))

(defun hyperdrive-fons-view-image--flip-map (map flip size)
  "Horizontally flip MAP according to FLIP and SIZE.
Destructively modifies and returns MAP."
  (when flip
    (pcase-dolist (`(,`(,type . ,coords) ,_id ,_plist) map)
      (pcase-exhaustive type
        ('rect
         (let ((x0 (- (car size) (cadr coords)))
               (y0 (cdar coords))
               (x1 (- (car size) (caar coords)))
               (y1 (cddr coords)))
           (setcar coords (cons x0 y0))
           (setcdr coords (cons x1 y1))))
        ('circle
         (setf (caar coords) (- (car size) (caar coords))))
        ('poly
         (dotimes (i (length coords))
           (when (= 0 (% i 2))
             (aset coords i (- (car size) (aref coords i)))))))))
  map)

(provide 'hyperdrive-fons-view)
;;; hyperdrive-fons-view.el ends here
