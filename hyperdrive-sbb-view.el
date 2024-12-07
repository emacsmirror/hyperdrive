;;; hyperdrive-sbb-view.el --- Visualize sbb relations  -*- lexical-binding: t; -*-

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

;; This library implements a graphviz-based view for `sbb' relations data.

;; Some of the code is copied from `org-graph-view':
;; https://github.com/alphapapa/org-graph-view, which is GPLv3+ licensed.

;;; Code:

;; TODO: Bookmark support

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'color)

(require 'hyperdrive-sbb)

;;;; Customization

(defgroup h/sbb-view nil
  "Visualize sbb relations."
  :group 'h/sbb)

(defcustom h/sbb-view-layout "dot"
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

(defcustom h/sbb-view-overlap "voronoi"
  "How to handle overlapping.  See Graphviz documentation.
It seems unclear which is the best default, because each option
renders one or another layout slightly better than other
options."
  :type '(choice (const :tag "Voronoi" "voronoi")
		 (const :tag "Scale" "scale")
		 (const :tag "Allow overlap" "true")
		 (const :tag "VPSC" "vpsc")))

(defface h/sbb-source '((t :inherit success))
  "Applied to sources.")

(defface h/sbb-blocker '((t :inherit font-lock-constant-face
                            :weight bold))
  "Applied to blockers.")

(defface h/sbb-blocked '((t :inherit error))
  "Applied to blocked.")

;; TODO: Reload image on defcustom change
(defcustom h/sbb-view-sources-color (face-foreground 'h/sbb-source nil t)
  "Source edge and node color.
May be any string listed here:
<https://graphviz.org/doc/info/colors.html>."
  ;; Don't use :type 'color.  Emacs colors don't necessarily match graphviz
  ;; colors, and they may contain spaces, preventing the graph from rendering.
  :type 'string)

(defcustom h/sbb-view-blockers-color (face-foreground 'h/sbb-blocker nil t)
  "Blocker edge and node color.
May be any string listed here:
<https://graphviz.org/doc/info/colors.html>."
  ;; Don't use :type 'color.  Emacs colors don't necessarily match graphviz
  ;; colors, and they may contain spaces, preventing the graph from rendering.
  :type 'string)

(defcustom h/sbb-view-blocked-color (face-foreground 'h/sbb-blocked nil t)
  "Blocked edge and node color.
May be any string listed here:
<https://graphviz.org/doc/info/colors.html>."
  ;; Don't use :type 'color.  Emacs colors don't necessarily match graphviz
  ;; colors, and they may contain spaces, preventing the graph from rendering.
  :type 'string)

;;;; Mode

(define-derived-mode h/sbb-view-mode special-mode
  `("Hyperdrive-sbb-view"
    ;; TODO: Add more to lighter, e.g. menu to change params.
    )
  "Major mode for viewing hyperdrive sbb graphs."
  :group 'hyperdrive
  :interactive nil
  ;; Don't scale with font pixel size.
  (setq-local image-scaling-factor 1)
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

(cl-defun h/sbb-view
    (relations root &key (layout h/sbb-view-layout) insert-relation-fun edge-url-format-fun)
  "View RELATIONS from ROOT."
  (h/sbb-view--render-graphviz
   (h/sbb-view--format-graph
    relations :root-name root :layout layout
    :window (get-buffer-window (current-buffer))
    :insert-relation-fun insert-relation-fun
    :edge-url-format-fun edge-url-format-fun)))

(defun h/sbb-view--graphviz (type)
  "Run Graphviz for TYPE on current buffer.
Graphviz is called on current buffer content, which should be a
graphviz string, and replaces it with the rendered output."
  (unless (zerop (call-process-region (point-min) (point-max) "dot" 'delete t nil
                                      (concat "-T" type)))
    (error "Error generating graph: %S" (buffer-string))))

(cl-defun h/sbb-view--render-graphviz (graphviz)
  "Render GRAPHVIZ string in current buffer."
  (let* ((original-map (h/sbb-view--graph-map graphviz))
         (svg-string (h/sbb-view--svg graphviz))
         (window-width
          (window-text-width (get-buffer-window (current-buffer)) t))
         (window-height
          (window-text-height (get-buffer-window (current-buffer)) t))
         (inhibit-read-only t)
         (image (create-image svg-string 'svg t
                              :original-map original-map
                              :max-width window-width
                              :max-height window-height
                              :width window-width
                              :height window-height)))
    (when (> 30 emacs-major-version)
      ;; TODO(deprecate-29): bug#69602 resolved in Emacs 30.
      (setq image
            (nconc image
                   (list
                    :map (h/sbb-view-image--compute-map image)))))
    (erase-buffer)
    (insert-image image)
    (goto-char (point-min))))

(defun h/sbb-view--graph-map (graph)
  "Return image map for Graphviz GRAPH."
  (with-temp-buffer
    (insert graph)
    (h/sbb-view--graphviz "cmapx")
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

(cl-defun h/sbb-view--format-graph
    (relations &key root-name layout window insert-relation-fun edge-url-format-fun)
  "Return a graphviz-string string for RELATIONS."
  (cl-labels ((insert-vals (&rest pairs)
                (cl-loop for (key value) on pairs by #'cddr
                         do (insert (format "%s=\"%s\"" key value) "\n")))
              (format-val-list (&rest pairs)
                (format "[%s]" (string-join
                                (cl-loop for (key value) on pairs by #'cddr
                                         collect (format "%s=\"%s\"" key value))
                                ",")))
              (format-hop (hop type)
                (pcase-let (((cl-struct h/sbb-hop from to) hop))
                  (format "%s -> %s [edgeURL=\"%s\", color=\"%s\"];\n"
                          from to (funcall edge-url-format-fun hop type)
                          (pcase type
                            ('sources h/sbb-view-sources-color)
                            ('blockers h/sbb-view-blockers-color)
                            ('blocked h/sbb-view-blocked-color)))))
              (format-to (to _relation)
                (funcall insert-relation-fun to relations root-name)))
    (with-temp-buffer
      (save-excursion
        (insert "digraph sbbrelationview {\n")
        (insert "edge" (format-val-list "color" (face-attribute 'default :foreground)) ";\n")
        (insert "node" (format-val-list "fontname" (face-attribute 'default :family)
				        "mindist" "1")
	        ";\n")
        (insert-vals "layout" layout
                     "bgcolor" (face-attribute 'default :background)
                     "overlap" h/sbb-view-overlap
                     "compound" "true"
                     ;; TODO: Comment: Look into using "size" with "ratio: fill"
                     ;; to make the node text of consistently readable size.
                     ;; "size" (format "%d,%d!"
                     ;;                (/ (window-text-width window t) 92.0)
                     ;;                (/ (window-text-height window t) 92.0))
                     ;; "ratio" "fill"
                     "ratio" (/ (window-text-height window t)
                                (window-text-width window t) 1.0)
                     "mindist" "0")
        (dolist (hop (h/sbb-relations-hops relations 'sources))
          (insert (format-hop hop 'sources)))
        (dolist (hop (h/sbb-relations-hops relations 'blockers))
          (insert (format-hop hop 'blockers)))
        (let ((blocker-paths-exist-p
               (catch 'blocker-paths-exist-p
                 ;; `relations' might be filtered to exclude blockers.  Only
                 ;; display blocked hops when blockers are displayed.  Blocked
                 ;; nodes may still be displayed without edges.
                 (maphash (lambda (_id relation)
                            (when (h/sbb-relation-blocker-paths relation)
                              (throw 'blocker-paths-exist-p t)))
                          relations))))
          (dolist (hop (h/sbb-relations-hops relations 'blocked))
            (when (or blocker-paths-exist-p
                      ;; NOTE: Without adding another argument to
                      ;; `h/sbb-view--format-graph', there's no way to
                      ;; distinguish between a `relations' table which has been
                      ;; filtered to remove blocker paths and a table which
                      ;; never had any blocker paths.  Therefore, this code
                      ;; always shows direct blocked hops from root, even when
                      ;; `hpg/show-blockers-p' is nil.
                      (equal root-name (h/sbb-hop-from hop)))
              (insert (format-hop hop 'blocked)))))
        (funcall insert-relation-fun root-name relations root-name)
        (insert (format "root=\"%s\"\n" root-name)) ; "twopi", "circo" only
        (maphash #'format-to relations)
        (insert "}"))
      ;; (message "%s" (buffer-string))
      (buffer-string))))

(cl-defun h/sbb-view--svg (graph)
  "Return SVG string for Graphviz GRAPH."
  (with-temp-buffer
    (insert graph)
    (h/sbb-view--graphviz "svg")
    (goto-char (point-min))
    (buffer-string)))

;; (defvar h/sbb-view-prism-minimum-contrast 6
;;   "Attempt to enforce this minimum contrast ratio for user faces.
;; This should be a reasonable number from, e.g. 0-7 or so."
;;   ;; Prot would almost approve of this default.  :) I would go all the way
;;   ;; to 7, but 6 already significantly dilutes the colors in some cases.
;;   )

;; (cl-defun h/sbb-view--prism-color
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
;;       (when (< (contrast-ratio color-rgb contrast-with-rgb) h/sbb-view-prism-minimum-contrast)
;;         (setf color-rgb (increase-contrast color-rgb contrast-with-rgb h/sbb-view-prism-minimum-contrast
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
              :after #'h/sbb-view--recompute-image-map-at-point)
  (advice-add #'image-rotate
              :after #'h/sbb-view--recompute-image-map-at-point)
  (advice-add #'image-flip-horizontally
              :after #'h/sbb-view--recompute-image-map-at-point)
  (advice-add #'image-flip-vertically
              :after #'h/sbb-view--recompute-image-map-at-point))

(defun h/sbb-view--recompute-image-map-at-point (&rest _args)
  "Recompute :map for image at point.
Intended as :after advice for commands which transform images."
  (when-let* ((image (image--get-image))
              (original-map (image-property image :original-map)))
    (setf (image-property image :map)
          (h/sbb-view-image--compute-map image))))

(defsubst h/sbb-view-image--compute-rotation (image)
  "Copy of `image--compute-rotation' from Emacs 30.
Accepts IMAGE."
  (let ((degrees (or (image-property image :rotation) 0)))
    (and (= 0 (mod degrees 1))
         (car (memql (truncate (mod degrees 360)) '(0 90 180 270))))))

(defun h/sbb-view-image--compute-map (image)
  "Copy of `image--compute-map' from Emacs 30.
Accepts IMAGE."
  (when-let* ((map (image-property image :original-map)))
    (setq map (copy-tree map t))
    (let* ((size (image-size image t))
           ;; The image can be scaled for many reasons (:scale,
           ;; :max-width, etc), so using `image--current-scaling' to
           ;; calculate the current scaling is the correct method.  But,
           ;; since each call to `image_size' is expensive, the code is
           ;; duplicated here to save the a call to `image-size'.
           (scale (/ (float (car size))
                     (car (image-size
                           (image--image-without-parameters image) t))))
           (rotation (h/sbb-view-image--compute-rotation image))
           ;; Image is flipped only if rotation is a multiple of 90,
           ;; including 0.
           (flip (and rotation (image-property image :flip))))
      ;; SIZE fits MAP after transformations.  Scale MAP before flip and
      ;; rotate operations, since both need MAP to fit SIZE.
      (unless (= scale 1)
        (h/sbb-view-image--scale-map map scale))
      ;; In rendered images, rotation is always applied before flip.
      (when (memql rotation '(90 180 270))
        (h/sbb-view-image--rotate-map
         map rotation (if (= rotation 180)
                          size
                        ;; If rotated ±90°, swap width and height.
                        (cons (cdr size) (car size)))))
      ;; After rotation, there's no need to swap width and height.
      (when flip
        (h/sbb-view-image--flip-map map size)))
    map))

;; (defun h/sbb-view-image--compute-original-map (image)
;;   "Copy of `image--compute-original-map' from Emacs 30.
;; Accepts IMAGE."
;;   (when-let* ((original-map (image-property image :map)))
;;     (setq original-map (copy-tree original-map t))
;;     (let* ((size (image-size image t))
;;            ;; The image can be scaled for many reasons (:scale,
;;            ;; :max-width, etc), so using `image--current-scaling' to
;;            ;; calculate the current scaling is the correct method.  But,
;;            ;; since each call to `image_size' is expensive, the code is
;;            ;; duplicated here to save the a call to `image-size'.
;;            (scale (/ (float (car size))
;;                      (car (image-size
;;                            (image--image-without-parameters image) t))))
;;            (rotation (h/sbb-view-image--compute-rotation image))
;;            ;; Image is flipped only if rotation is a multiple of 90
;;            ;; including 0.
;;            (flip (and rotation (image-property image :flip))))
;;       ;; In rendered images, rotation is always applied before flip.
;;       ;; To undo the transformation, flip before rotating.  SIZE fits
;;       ;; ORIGINAL-MAP before transformations are applied.  Therefore,
;;       ;; scale ORIGINAL-MAP after flip and rotate operations, since
;;       ;; both need ORIGINAL-MAP to fit SIZE.
;;       ;; In rendered images, rotation is always applied before flip.
;;       (when flip
;;         (h/sbb-view-image--flip-map original-map size))
;;       (when (memql rotation '(90 180 270))
;;         (h/sbb-view-image--rotate-map original-map (- rotation) size))
;;       (unless (= scale 1)
;;         (h/sbb-view-image--scale-map original-map (/ 1.0 scale))))
;;     original-map))

(defun h/sbb-view-image--scale-map (map scale)
  "Copy of `image--scale-map' from Emacs 30.
Accepts IMAGE and SCALE."
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
               (round (* (aref coords i) scale)))))))
  map)

(defun h/sbb-view-image--rotate-map (map rotation size)
  "Copy of `image--rotate-map' from Emacs 30.
Accepts MAP, ROTATION, and SIZE."
  (setq rotation (mod rotation 360))
  (pcase-dolist (`(,`(,type . ,coords) ,_id ,_plist) map)
    (pcase-exhaustive type
      ('rect
       (let ( x0 y0  ; New upper left corner
              x1 y1) ; New bottom right corner
         (pcase rotation ; Set new corners to...
           (90 ; ...old bottom left and upper right
            (setq x0 (caar coords) y0 (cddr coords)
                  x1 (cadr coords) y1 (cdar coords)))
           (180 ; ...old bottom right and upper left
            (setq x0 (cadr coords) y0 (cddr coords)
                  x1 (caar coords) y1 (cdar coords)))
           (270 ; ...old upper right and bottom left
            (setq x0 (cadr coords) y0 (cdar coords)
                  x1 (caar coords) y1 (cddr coords))))
         (setcar coords (h/sbb-view-image--rotate-coord x0 y0 rotation size))
         (setcdr coords (h/sbb-view-image--rotate-coord x1 y1 rotation size))))
      ('circle
       (setcar coords (h/sbb-view-image--rotate-coord
                       (caar coords) (cdar coords) rotation size)))
      ('poly
       (dotimes (i (length coords))
         (when (= 0 (% i 2))
           (pcase-let ((`(,x . ,y)
                        (h/sbb-view-image--rotate-coord
                         (aref coords i) (aref coords (1+ i)) rotation size)))
             (aset coords i x)
             (aset coords (1+ i) y)))))))
  map)

(defun h/sbb-view-image--rotate-coord (x y angle size)
  "Copy of `image--rotate-coord' from Emacs 30.
Accepts X, Y, ANGLE, and SIZE."
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

(defun h/sbb-view-image--flip-map (map size)
  "Copy of `image--flip-map' from Emacs 30.
Accepts MAP and SIZE."
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
           (aset coords i (- (car size) (aref coords i))))))))
  map)

;;;; Footer

(provide 'hyperdrive-sbb-view)

;; Local Variables:
;; read-symbol-shorthands: (
;;   ("he//" . "hyperdrive-entry--")
;;   ("he/"  . "hyperdrive-entry-")
;;   ("h//"  . "hyperdrive--")
;;   ("h/"   . "hyperdrive-"))
;; End:

;;; hyperdrive-sbb-view.el ends here
