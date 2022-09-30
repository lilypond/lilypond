;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2003--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(use-modules (ice-9 match))

(define (make-bezier-sandwich-stencil coords thick)
  (make-path-stencil
   (match coords
     (((x1 . y1) (x2 . y2) (x3 . y3) (x4 . y4) (x5 . y5) (x6 . y6))
      `(moveto ,x1 ,y1
               curveto ,x2 ,y2 ,x3 ,y3 ,x4 ,y4
               curveto ,x5 ,y5 ,x6 ,y6 ,x1 ,y1
               closepath)))
   thick
   1
   1
   #t))

(define-public (make-bow-stencil
                start stop thickness angularity bow-height orientation)
  "Create a bow stencil.
It starts at point @var{start}, ends at point @var{stop}.
@var{thickness} is the thickness of the bow.
The higher the value of number @var{angularity}, the more angular the shape of
the bow.
@var{bow-height} determines the height of the bow.
@var{orientation} determines whether the bow is concave or convex.
Both variables are supplied to support independent usage.

Done by calculating a horizontal unit bow first, then moving all control points
to the correct positions.
Limitation: s-curves are currently not supported.
"

;;;; Coding steps:
;;;; (1) calculate control-points for a "unit"-bow from '(0 . 0) to '(1 . 0)
;;;;     user settable `bow-height' and `thickness' are scaled down.
;;;; (2) move control-points to match `start' and `stop'

  (let* (;; we use a fixed line-width as border for different behaviour
         ;; for larger and (very) small lengths
         (line-width 0.1)
         ;; `start'-`stop' distances
         (dx (- (car stop) (car start)))
         (dy (- (cdr stop) (cdr start)))
         (length-to-print (magnitude (make-rectangular dx dy))))

    (if (= 0 length-to-print)
        empty-stencil
        (let* (
          ;;;; (1) calculate control-points for the horizontal unit-bow,
               ;; y-values for 2nd/3rd control-points
               (outer-control
                (* 4/3 (sign orientation) (/ bow-height length-to-print)))
               (inner-control
                (* (sign orientation)
                   (- (abs outer-control) (/ thickness length-to-print))))
               ;; x-values for 2nd/3rd control-points depending on `angularity'
               (offset-index
                (- (* 0.6 angularity) 0.8))
               (left-control
                (+ 0.1 (* 0.3 angularity)))
               (right-control
                (- 1 left-control))
               ;; defining 2nd and 3rd outer control-points
               (left-outer-control-point
                (cons left-control outer-control))
               (right-outer-control-point
                (cons right-control outer-control))
               ;; defining 2nd and 3rd inner control-points
               (left-inner-control-point
                (cons left-control inner-control))
               (right-inner-control-point
                (cons right-control inner-control))
               (coord-list
                (list
                 '(0 . 0)
                 left-outer-control-point
                 right-outer-control-point
                 '(1 . 0)
                 right-inner-control-point
                 left-inner-control-point))
               ;;;; (2) move control-points to match `start' and `stop'
               (moved-coord-list
                (map
                 (lambda (p)
                   (cons
                    (+ (car start) (- (* (car p) dx) (* (cdr p) dy)))
                    (+ (cdr start) (+ (* (car p) dy) (* (cdr p) dx)))))
                 coord-list)))

          ;; final stencil
          (make-bezier-sandwich-stencil
           moved-coord-list
           (min (* 2 thickness) line-width))))))

(define*-public (make-tie-stencil
                 start stop thickness orientation
                 #:optional (height-limit 1.0)(ratio 0.33)(angularity 0.5))
  (let* (;; taken from bezier-bow.cc
         (F0_1
          (lambda (x) (* (/ 2 PI) (atan (* PI x 0.5)))))
         (slur-height
          (lambda (w h_inf r_0) (* (F0_1 (/ (* w r_0) h_inf)) h_inf)))
         (width (abs (- (car start) (car stop))))
         (height (slur-height width height-limit ratio)))
    (make-bow-stencil start stop thickness angularity height orientation)))

(define-public (stack-stencils axis dir padding stils)
  "Stack stencils @var{stils} in direction @var{axis}, @var{dir}, using
@var{padding}."
  (reduce
   (lambda (next front)
     (ly:stencil-stack front axis dir next padding))
   empty-stencil
   stils))

(define-public (stack-stencils-padding-list axis dir paddings stils)
  "Stack stencils @var{stils} in direction @var{axis}, @var{dir}, using
a list of @var{paddings}."
  (if (null? stils)
      empty-stencil
      (fold
       (lambda (next padding front)
         (let ((offset (+ (- (interval-end (ly:stencil-extent front axis))
                             (interval-start (ly:stencil-extent next axis)))
                          padding)))
           (ly:stencil-add
            front
            (ly:stencil-translate-axis next offset axis))))
       (car stils)
       (cdr stils)
       paddings)))

(define-public (centered-stencil stencil)
  "Center stencil @var{stencil} in both the x and y@tie{}directions."
  (ly:stencil-aligned-to (ly:stencil-aligned-to stencil X CENTER) Y CENTER))

(define-public (stack-lines dir padding baseline stils)
  "Stack stencils vertically with a baseline skip."
  (reduce-right
   (lambda (next back) (ly:stencil-stack next Y dir back padding baseline))
   empty-stencil
   (map
    (lambda (s)
      ;; X-empty stencils may add vertical space.  A stencil that is
      ;; merely Y-empty counts as horizontal spacing.  Since we want
      ;; those to register as lines of their own (is this a good
      ;; idea?), we make them a separately visible line.
      (if (and (ly:stencil-empty? s Y)
               (not (ly:stencil-empty? s X)))
          (ly:make-stencil (ly:stencil-expr s) (ly:stencil-extent s X) '(0 . 0))
          s))
    stils)))

(define-public (bracketify-stencil stil axis thick protrusion padding)
  "Add brackets around @var{stil}, producing a new stencil."

  (let* ((ext (ly:stencil-extent stil axis))
         (lb (ly:bracket axis ext thick protrusion))
         (rb (ly:bracket axis ext thick (- protrusion))))
    (set! stil
          (ly:stencil-combine-at-edge stil (other-axis axis) 1 rb padding))
    (set! stil
          (ly:stencil-combine-at-edge stil (other-axis axis) -1 lb padding))
    stil))

(define (make-parenthesis-stencil
         y-extent thickness width angularity orientation)
  "Create a parenthesis stencil.
@var{y-extent} is the Y extent of the markup inside the parenthesis.
@var{half-thickness} is the half thickness of the parenthesis.
@var{width} is the width of a parenthesis.
@var{orientation} is the orientation of a parenthesis.
The higher the value of number @var{angularity},
the more angular the shape of the parenthesis."
  (let* ((start (cons 0 (car y-extent)))
         (stop (cons 0 (cdr y-extent)))
         (line-width 0.1)
         (bow-stil
          (make-bow-stencil
           start stop thickness angularity width orientation))
         (x-extent (ly:stencil-extent bow-stil X)))
    (ly:make-stencil
     (ly:stencil-expr bow-stil)
     (interval-widen x-extent (/ line-width 2))
     (interval-widen y-extent (/ line-width 2)))))

(define-public (parenthesize-stencil
                stencil half-thickness width angularity padding)
  "Add parentheses around @var{stencil}, returning a new stencil."
  (let* ((y-extent (ly:stencil-extent stencil Y))
         (lp (make-parenthesis-stencil
              y-extent half-thickness width angularity 1))
         (rp (make-parenthesis-stencil
              y-extent half-thickness width angularity -1)))
    (set! stencil (ly:stencil-combine-at-edge stencil X LEFT lp padding))
    (set! stencil (ly:stencil-combine-at-edge stencil X RIGHT rp padding))
    stencil))

(define-public (make-line-stencil width startx starty endx endy)
  "Make a line stencil of given line width and set its extents accordingly."
  (let ((xext (cons (min startx endx) (max startx endx)))
        (yext (cons (min starty endy) (max starty endy))))
    (ly:make-stencil
     (list 'draw-line width startx starty endx endy)
     ;; Since the line has rounded edges, we have to / can safely add half the
     ;; width to all coordinates!
     (interval-widen xext (/ width 2))
     (interval-widen yext (/ width 2)))))

(define-public (make-transparent-box-stencil xext yext)
  "Make a transparent box."
  (ly:stencil-outline empty-stencil (make-filled-box-stencil xext yext)))

(define-public (make-filled-box-stencil xext yext)
  "Make a filled box."

  (ly:make-stencil
   (list 'round-filled-box (- (car xext)) (cdr xext)
         (- (car yext)) (cdr yext) 0.0)
   xext yext))

(define-public (make-circle-stencil radius thickness fill)
  "Make a circle of radius @var{radius} and thickness @var{thickness}."
  (let*
      ((out-radius (+ radius (/ thickness 2.0))))

    (ly:make-stencil
     (list 'circle radius thickness fill)
     (cons (- out-radius) out-radius)
     (cons (- out-radius) out-radius))))

(define-public (make-oval-stencil x-radius y-radius thickness fill)
  "Make an oval from two Bézier curves, of x@tie{}radius @var{x-radius},
y@tie{}radius @var{y-radius}, and thickness @var{thickness} with fill
defined by @var{fill}."
  (let*
      ((x-out-radius (+ x-radius (/ thickness 2.0)))
       (y-out-radius (+ y-radius (/ thickness 2.0)))
       (x-max x-radius)
       (x-min (- x-radius))
       (y-max y-radius)
       (y-min (- y-radius))
       (commands `(,(list 'moveto x-max 0)
                   ,(list 'curveto x-max y-max x-min y-max x-min 0)
                   ,(list 'curveto x-min y-min x-max y-min x-max 0)
                   ,(list 'closepath)))
       (command-list (fold-right append '() commands)))
    (ly:make-stencil
     `(path ,thickness ,command-list round round ,fill)
     (cons (- x-out-radius) x-out-radius)
     (cons (- y-out-radius) y-out-radius))))

(define-public (make-partial-ellipse-stencil
                x-radius y-radius start-angle end-angle thick connect fill)
  "Create an elliptical arc.
@var{x-radius} is the x@tie{}radius of the arc.
@var{y-radius} is the y@tie{}radius of the arc.
@var{start-angle} is the starting angle of the arc (in degrees).
@var{end-angle} is the ending angle of the arc (in degrees).
@var{thick} is the thickness of the line.
@var{connect} is a boolean flag indicating whether the end should
be connected to the start by a line.
@var{fill} is a boolean flag indicating whether the shape should be filled."
  (define (make-radius-list x-radius y-radius)
    "Makes a list of angle/radius pairs at intervals of PI/2 for
the partial ellipse until 7*PI/2.  For example, in pseudo-code:
> (make-radius-list 2 3)\
\n((0.0 . 2) (PI/2 . 3) (PI . -2) (3*PI/2 . -3)\
\n(2*PI . 2) (5*PI/2 . 3) (3*PI . -2) (7*PI/2 . -3))
"
    (append-map
     (lambda (adder)
       (map (lambda (quadrant)
              (cons (+ adder (car quadrant))
                    (cdr quadrant)))
            `((0.0 . (,x-radius . 0.0))
              (,PI-OVER-TWO . (0.0 . ,y-radius))
              (,PI . (,(- x-radius) . 0.0))
              (,THREE-PI-OVER-TWO . (0.0 . ,(- y-radius))))))
     `(0.0 ,TWO-PI)))

  (define
    (insert-in-ordered-list ordering-function value inlist cutl? cutr?)
    "Insert @var{value} in ordered list @var{inlist}. If @var{cutl?}, we
cut away any parts of @var{inlist} before @var{value}. @var{cutr?} works
the same way but for the right side. For example:
> (insert-in-ordered-list < 4 '(1 2 3 6 7) #f #f)
'(1 2 3 4 6 7)
> (insert-in-ordered-list < 4 '(1 2 3 6 7) #t #f)
'(4 6 7)
> (insert-in-ordered-list < 4 '(1 2 3 6 7) #f #t)
'(1 2 3 4)
"
    (define
      (helper ordering-function value left-list right-list cutl? cutr?)
      (if (null? right-list)
          (append
           (if cutl? '() left-list)
           (list value)
           (if cutr? '() right-list))
          (if (ordering-function value (car right-list))
              (append
               (if cutl? '() left-list)
               (list value)
               (if cutr? '() right-list))
              (helper
               ordering-function
               value
               (append left-list (list (car right-list)))
               (cdr right-list)
               cutl?
               cutr?))))
    (helper ordering-function value '() inlist cutl? cutr?))

  (define (ordering-function-1 a b) (car< a b))

  (define (ordering-function-2 a b) (car<= a b))

  (define (min-max-crawler min-max side l)
    "Apply function @var{side} to each member of list and
then reduce using @var{min-max}:
> (min-max-crawler min car '((0 . 3) (-1 . 4) (1 . 2)))
-1
> (min-max-crawler min cdr '((0 . 3) (-1 . 4) (1 . 2)))
2
"
    (reduce min-max
            (if (eq? min-max min) 100000 -100000)
            (map side l)))

  (let*
      (;; the outside limit of the x-radius
       (x-out-radius (+ x-radius (/ thick 2.0)))
       ;; the outside limit of the y-radius
       (y-out-radius (+ y-radius (/ thick 2.0)))
       ;; end angle to radians
       (new-end-angle (angle-0-2pi (degrees->radians end-angle)))
       ;; length of the radius at the end angle
       (end-radius (ellipse-radius x-out-radius y-out-radius new-end-angle))
       ;; start angle to radians
       (new-start-angle (angle-0-2pi (degrees->radians start-angle)))
       ;; length of the radius at the start angle
       (start-radius (ellipse-radius x-out-radius y-out-radius new-start-angle))
       ;; points that the arc passes through at 90 degree intervals
       (radius-list (make-radius-list x-out-radius y-out-radius))
       ;; rectangular coordinates of arc endpoint
       (rectangular-end-radius (polar->rectangular end-radius end-angle))
       ;; rectangular coordinates of arc begin point
       (rectangular-start-radius (polar->rectangular start-radius start-angle))
       ;; we want the end angle to always be bigger than the start angle
       ;; so we redefine it here just in case it is less
       (new-end-angle
        (if (<= new-end-angle new-start-angle)
            (+ TWO-PI new-end-angle)
            new-end-angle))
       ;; all the points that may be extrema of the arc
       ;; this is the 90 degree points plus the beginning and end points
       ;; we use this to calculate extents
       (possible-extrema
        (insert-in-ordered-list
         ordering-function-2
         (cons new-end-angle rectangular-end-radius)
         (insert-in-ordered-list
          ordering-function-1
          (cons new-start-angle rectangular-start-radius)
          radius-list
          #t
          #f)
         #f
         #t)))
    (ly:make-stencil
     (list
      'partial-ellipse
      x-radius
      y-radius
      start-angle
      end-angle
      thick
      connect
      fill)
     ;; we know the extrema points by crawling through the
     ;; list of possible extrema and finding the min and max
     ;; for x and y
     (cons (min-max-crawler min cadr possible-extrema)
           (min-max-crawler max cadr possible-extrema))
     (cons (min-max-crawler min cddr possible-extrema)
           (min-max-crawler max cddr possible-extrema)))))


(define*-public (make-path-stencil path thickness x-scale y-scale fill
                                   #:key (line-cap-style 'round)
                                   (line-join-style 'round))
  "Make a stencil based on the path described by the list @var{path}, with
thickness @var{thickness}, and scaled by @var{x-scale} in the x@tie{}direction
and @var{y-scale} in the y@tie{}direction (the difference with scaling the
resulting stencil using @code{ly:stencil-scale} is that this scaling does not
change the thickness).  @var{fill} is a boolean argument that specifies whether
the path should be filled.  Valid path commands are

@example
moveto rmoveto lineto rlineto curveto rcurveto closepath
@end example

and their standard SVG single-letter equivalents

@example
M m L l C c Z z
@end example"
  ;; Convert coordinates to absolute, so we can compute the extents.  We leave
  ;; them as absolute in the result (TODO: this probably means some bits of code
  ;; can be removed in the output backends.)
  (let loop ((path
              ;; A path must begin with a moveto.
              (match path
                ((moveto . _)
                 path)
                (_
                 (cons* 'moveto 0 0 path))))
             ;; The start of the current path.  Changed by moveto commands, used
             ;; for closepath.
             (origin-x 0)
             (origin-y 0)
             ;; The point at which the previous command left us.
             (previous-x 0)
             (previous-y 0)
             ;; Extents of the path so far.
             (X-extent empty-interval)
             (Y-extent empty-interval)
             ;; Absolute path.
             (acc '()))
    (define (handle-moveto x y rest)
      (loop rest
            ;; Reset origin.
            x
            y
            ;; Also reset previous point.
            x
            y
            ;; Extents are unchanged.
            X-extent
            Y-extent
            (cons `(moveto ,x ,y)
                  acc)))
    (define (handle-lineto x y rest)
      (loop rest
            origin-x
            origin-y
            ;; End point of the line becomes the new previous point.
            x y
            ;; Account for the line's extents.
            (interval-union X-extent (ordered-cons previous-x x))
            (interval-union Y-extent (ordered-cons previous-y y))
            (cons `(lineto ,x ,y)
                  acc)))
    (define (handle-curveto x2 y2 x3 y3 x4 y4 rest)
      (let ((bezier `((,previous-x . ,previous-y) (,x2 . ,y2) (,x3 . ,y3) (,x4 . ,y4))))
        (loop rest
              origin-x
              origin-y
              ;; End point of the curve becomes the new previous point.
              x4
              y4
              ;; Account for the curve's extents.
              (interval-union X-extent (ly:bezier-extent bezier X))
              (interval-union Y-extent (ly:bezier-extent bezier Y))
              (cons `(curveto ,x2 ,y2 ,x3 ,y3 ,x4 ,y4)
                    acc))))
    (define (handle-closepath rest)
      (loop rest
            origin-x
            origin-y
            ;; Origin becomes the new previous point again.
            origin-x
            origin-y
            ;; Account for the line's extents.
            (interval-union X-extent (ordered-cons previous-x origin-x))
            (interval-union Y-extent (ordered-cons previous-y origin-y))
            (cons '(closepath)
                  acc)))
    (match path
      (((or 'moveto 'M) x y . rest)
       (handle-moveto (* x x-scale)
                      (* y y-scale)
                      rest))
      (((or 'rmoveto 'm) x y . rest)
       (handle-moveto (+ previous-x (* x x-scale))
                      (+ previous-y (* y y-scale))
                      rest))
      (((or 'lineto 'L) x y . rest)
       (handle-lineto (* x x-scale)
                      (* y y-scale)
                      rest))
      (((or 'rlineto 'l) x y . rest)
       (handle-lineto (+ previous-x (* x x-scale))
                      (+ previous-y (* y y-scale))
                      rest))
      (((or 'curveto 'C) x2 y2 x3 y3 x4 y4 . rest)
       ;; Conceptually, x1 and y1 are the current point.
       (handle-curveto (* x2 x-scale)
                       (* y2 y-scale)
                       (* x3 x-scale)
                       (* y3 y-scale)
                       (* x4 x-scale)
                       (* y4 y-scale)
                       rest))
      (((or 'rcurveto 'c) x2 y2 x3 y3 x4 y4 . rest)
       (handle-curveto (+ previous-x (* x2 x-scale))
                       (+ previous-y (* y2 y-scale))
                       (+ previous-x (* x3 x-scale))
                       (+ previous-y (* y3 y-scale))
                       (+ previous-x (* x4 x-scale))
                       (+ previous-y (* y4 y-scale))
                       rest))
      (((or 'closepath 'Z 'z) . rest)
       (handle-closepath rest))
      (()
       (let ((half-thickness (/ thickness 2)))
         (ly:make-stencil
          `(path ,thickness
                 ;; TODO: a nested internal representation '((moveto x y) (lineto x y))
                 ;; might be better than this flat representation '(moveto x y lineto x y).
                 ,(apply append (reverse! acc))
                 ,line-cap-style
                 ,line-join-style
                 ,fill)
          (interval-widen X-extent half-thickness)
          (interval-widen Y-extent half-thickness)))))))

(define-public (make-connected-path-stencil pointlist thickness
                                            x-scale y-scale connect fill)
  "Make a connected path described by the list @var{pointlist}, beginning
at point @w{(0, 0)}, with thickness @var{thickness}, and scaled by
@var{x-scale} in the x@tie{}direction and @var{y-scale} in the
y@tie{}direction.  @var{connect} and @var{fill} are boolean arguments that
specify whether the path should be connected or filled, respectively."
  (make-path-stencil
   (concatenate
    (append
     (map (lambda (path-unit)
            (case (length path-unit)
              ((2) (append (list 'lineto) path-unit))
              ((6) (append (list 'curveto) path-unit))))
          pointlist)
     ;; if this path is connected, add closepath to the end
     (if connect (list '(closepath)) '())))
   thickness x-scale y-scale fill))

(define-public (make-ellipse-stencil x-radius y-radius thickness fill)
  "Make an ellipse of x@tie{}radius @var{x-radius}, y@tie{}radius
@code{y-radius}, and thickness @var{thickness} with fill defined by
@code{fill}."
  (let*
      ((x-out-radius (+ x-radius (/ thickness 2.0)))
       (y-out-radius (+ y-radius (/ thickness 2.0))) )

    (ly:make-stencil
     (list 'ellipse x-radius y-radius thickness fill)
     (cons (- x-out-radius) x-out-radius)
     (cons (- y-out-radius) y-out-radius))))

(define-public (box-grob-stencil grob)
  "Make a box of exactly the extents of the grob.  The box precisely
encloses the contents."
  (let* ((xext (ly:grob-extent grob grob 0))
         (yext (ly:grob-extent grob grob 1))
         (thick 0.01))

    (ly:stencil-add
     (make-filled-box-stencil xext (cons (- (car yext) thick) (car yext)))
     (make-filled-box-stencil xext (cons (cdr yext) (+ (cdr yext) thick)))
     (make-filled-box-stencil (cons (cdr xext) (+ (cdr xext) thick)) yext)
     (make-filled-box-stencil (cons (- (car xext) thick) (car xext)) yext))))

;; TODO merge this and prev function.
(define-public (box-stencil stencil thickness padding)
  "Add a box around @var{stencil}, producing a new stencil."
  (let* ((x-ext (interval-widen (ly:stencil-extent stencil 0) padding))
         (y-ext (interval-widen (ly:stencil-extent stencil 1) padding))
         (y-rule (make-filled-box-stencil (cons 0 thickness) y-ext))
         (x-rule (make-filled-box-stencil
                  (interval-widen x-ext thickness) (cons 0 thickness))))
    (set! stencil (ly:stencil-combine-at-edge stencil X 1 y-rule padding))
    (set! stencil (ly:stencil-combine-at-edge stencil X -1 y-rule padding))
    (set! stencil (ly:stencil-combine-at-edge stencil Y 1 x-rule 0.0))
    (set! stencil (ly:stencil-combine-at-edge stencil Y -1 x-rule 0.0))
    stencil))

(define-public (circle-stencil stencil thickness padding)
  "Add a circle around @var{stencil}, producing a new stencil."
  (let* ((x-ext (ly:stencil-extent stencil X))
         (y-ext (ly:stencil-extent stencil Y))
         (diameter (max (interval-length x-ext)
                        (interval-length y-ext)))
         (radius (+ (/ diameter 2) padding thickness))
         (circle (make-circle-stencil radius thickness #f)))

    (ly:stencil-add
     stencil
     (ly:stencil-translate circle
                           (cons
                            (interval-center x-ext)
                            (interval-center y-ext))))))

(define-public (oval-stencil stencil thickness x-padding y-padding)
  "Add an oval around @code{stencil}, padded by the padding pair,
producing a new stencil."
  (let* ((x-ext (ly:stencil-extent stencil X))
         (y-ext (ly:stencil-extent stencil Y))
         (x-length (+ (interval-length x-ext) x-padding thickness))
         (y-length (+ (interval-length y-ext) y-padding thickness))
         (x-radius (* 0.707 x-length) )
         (y-radius (* 0.707 y-length) )
         (oval (make-oval-stencil x-radius y-radius thickness #f)))

    (ly:stencil-add
     stencil
     (ly:stencil-translate oval
                           (cons
                            (interval-center x-ext)
                            (interval-center y-ext))))))

(define-public (ellipse-stencil stencil thickness x-padding y-padding)
  "Add an ellipse around @var{stencil}, padded by the padding pair,
producing a new stencil."
  (let* ((x-ext (ly:stencil-extent stencil X))
         (y-ext (ly:stencil-extent stencil Y))
         (x-length (+ (interval-length x-ext) x-padding thickness))
         (y-length (+ (interval-length y-ext) y-padding thickness))
         ;; (aspect-ratio (/ x-length y-length))
         (x-radius (* 0.707 x-length) )
         (y-radius (* 0.707 y-length) )
         ;; (diameter (max (- (cdr x-ext) (car x-ext))
         ;;             (- (cdr y-ext) (car y-ext))))
         ;; radius (+ (/ diameter 2) padding thickness))
         (ellipse (make-ellipse-stencil x-radius y-radius thickness #f)))

    (ly:stencil-add
     stencil
     (ly:stencil-translate ellipse
                           (cons
                            (interval-center x-ext)
                            (interval-center y-ext))))))

(define-public (rounded-box-stencil stencil thickness padding blot)
  "Add a rounded box around @var{stencil}, producing a new stencil."

  (let* ((xext (interval-widen (ly:stencil-extent stencil 0) padding))
         (yext (interval-widen (ly:stencil-extent stencil 1) padding))
         (min-ext (min (-(cdr xext) (car xext)) (-(cdr yext) (car yext))))
         (ideal-blot (min blot (/ min-ext 2)))
         (ideal-thickness (min thickness (/ min-ext 2)))
         (outer (ly:round-filled-box
                 (interval-widen xext ideal-thickness)
                 (interval-widen yext ideal-thickness)
                 ideal-blot))
         (inner (ly:stencil-in-color
                 (ly:round-filled-box
                  xext yext (- ideal-blot ideal-thickness))
                 "white")))
    (ly:stencil-add outer inner stencil)))

(define-public (flip-stencil axis stil)
  "Flip stencil @var{stil} in the direction of @var{axis}.
Value @code{X} (or @code{0}) for @var{axis} flips it horizontally.
Value @code{Y} (or @code{1}) flips it vertically.  @var{stil} is
flipped in place; its position, the coordinates of its bounding
box, remains the same."
  (let* (
         ;; scale stencil using -1 to flip it and
         ;; then restore it to its original position
         (xy (if (= axis X) '(-1 . 1) '(1 . -1)))
         (flipped-stil (ly:stencil-scale stil (car xy) (cdr xy)))
         (flipped-ext (ly:stencil-extent flipped-stil axis))
         (original-ext (ly:stencil-extent stil axis))
         (offset (- (car original-ext) (car flipped-ext)))
         (replaced-stil (ly:stencil-translate-axis flipped-stil offset axis)))
    replaced-stil))

(define-public (stencil-with-color stencil color)
  (cond
   ((string? color) (ly:stencil-in-color stencil color))
   (color (apply ly:stencil-in-color stencil color))
   (else stencil)))

(define*-public (stencil-whiteout-outline
                 stil #:optional (thickness 0.3) (color white)
                 (angle-increments 16) (radial-increments 1))
  "White-out a stencil by surrounding it with white (or @var{color}) around
its outline.

This function works by creating a series of white or @var{color}
stencils radially offset from the original stencil with angles from
0 to 2*pi, at an increment of @code{angle-inc}, and with radii
from @code{radial-inc} to @var{thickness}.  @var{thickness} is how big
the white outline is, as a multiple of @code{line-thickness}.
@var{radial-increments} is how many copies of the white stencil we make
on our way out to thickness.  @var{angle-increments} is how many copies
of the white stencil we make between 0 and 2*pi."
  (if (or (not (positive? angle-increments))
          (not (positive? radial-increments)))
      (begin
        (ly:warning (G_ "Both angle-increments and radial-increments must be positive numbers."))
        stil)
      (let* ((angle-inc (/ 360 angle-increments))
             (radial-inc (/ thickness radial-increments)))

        (define (circle-plot ang dec radius original-stil new-stil)
          ;; ang (angle) and dec (decrement) are in degrees, not radians
          (if (<= ang 0)
              new-stil
              (circle-plot (- ang dec) dec radius original-stil
                           (ly:stencil-add
                            new-stil
                            (ly:stencil-translate original-stil
                                                  (ly:directed ang radius))))))

        (define (radial-plot radius original-stil new-stil)
          (if (<= radius 0)
              new-stil
              (ly:stencil-add new-stil
                              (radial-plot
                               (- radius radial-inc)
                               original-stil
                               (circle-plot 360 angle-inc
                                            radius original-stil empty-stencil)))))

        (let ((whiteout-expr
               (ly:stencil-expr
                (stencil-with-color
                 (radial-plot thickness stil empty-stencil)
                 color))))
          (ly:stencil-add
           (ly:make-stencil
            `(delay-stencil-evaluation ,(delay whiteout-expr)))
           stil)))))

(define*-public (stencil-whiteout-box stil
                                      #:optional (thickness 0) (blot 0)
                                      (color white))
  "White-out a stencil by printing it on top of a white (or @var{color})
rectangle.

@var{thickness} is how far, as a multiple of @code{line-thickness},
the white outline extends past the extents of stencil @var{stil}."
  (let*
      ((x-ext (interval-widen (ly:stencil-extent stil X) thickness))
       (y-ext (interval-widen (ly:stencil-extent stil Y) thickness)))

    (ly:stencil-add
     (stencil-with-color (ly:round-filled-box x-ext y-ext blot) color)
     stil)))

(define*-public (stencil-whiteout stil
                                  #:optional style thickness
                                  (line-thickness 0.1))
  "White-out a stencil (i.e., add a white background around it).

@var{style}, @var{thickness} and @var{line-thickness} are optional
arguments.  If set, @var{style} determines the shape of the white
background.  Given @code{'outline} the white background is produced
by @code{stencil-whiteout-outline}, given @code{'rounded-box} it is
produced by @code{stencil-whiteout-box} with rounded corners, given
other arguments (e.g., @code{'box}) or when unspecified it defaults to
@code{stencil-whiteout-box} with square corners.  If @var{thickness} is
specified it determines how far, as a multiple of @var{line-thickness},
the white background extends past the extents of stencil @var{stil}.  If
@var{thickness} has not been specified, an appropriate default is chosen
based on @var{style}."
  (let ((thick (* line-thickness
                  (if (number? thickness)
                      thickness
                      (cond
                       ((eq? style 'outline) 3)
                       ((eq? style 'rounded-box) 3)
                       (else 0))))))
    (cond
     ((eq? style 'special) stil)
     ((eq? style 'outline) (stencil-whiteout-outline stil thick))
     ((eq? style 'rounded-box) (stencil-whiteout-box stil thick (* 2 thick)))
     (else (stencil-whiteout-box stil thick)))))

(define-public (arrow-stencil-maker start? end?)
  "Return a function drawing a line from current point to @code{destination},
with optional arrows of @code{max-size} on start and end controlled by
@var{start?} and @var{end?}."
  (lambda (destination max-size)
    (let*
        ((e_x 1+0i)
         (e_y 0+1i)
         (distance (sqrt (+ (* (car destination) (car destination))
                            (* (cdr destination) (cdr destination)))))
         (size (min max-size (/ distance 3)))
         (rotate (lambda (z ang)
                   (* (make-polar 1 ang)
                      z)))
         (complex-to-offset (lambda (z)
                              (list (real-part z) (imag-part z))))

         (z-dest (+ (* e_x (car destination)) (* e_y (cdr destination))))
         (e_z (/ z-dest (magnitude z-dest)))
         (triangle-points (list
                           (* size -1+0.25i)
                           0
                           (* size -1-0.25i)))
         (p1s (map (lambda (z)
                     (+ z-dest (rotate z (angle z-dest))))
                   triangle-points))
         (p2s (map (lambda (z)
                     (rotate z (angle (- z-dest))))
                   triangle-points))
         (null (cons 0 0))
         (arrow-1
          (ly:make-stencil
           `(polygon  ,(append-map complex-to-offset p1s)
                      0.0
                      #t) null null))
         (arrow-2
          (ly:make-stencil
           `(polygon ,(append-map complex-to-offset p2s)
                     0.0
                     #t) null null ) )
         (thickness (min (/ distance 12) 0.1))
         (shorten-line (min (/ distance 3) 0.5))
         (start (complex-to-offset (/ (* e_z shorten-line) 2)))
         (end (complex-to-offset (- z-dest (/ (* e_z shorten-line) 2))))

         (line (ly:make-stencil
                `(draw-line ,thickness
                            ,(car start) ,(cadr start)
                            ,(car end) ,(cadr end)
                            )
                (cons (min 0 (car destination))
                      (min 0 (cdr destination)))
                (cons (max 0 (car destination))
                      (max 0 (cdr destination)))))

         (result
          (ly:stencil-add
           (if start? arrow-2 empty-stencil)
           (if end? arrow-1 empty-stencil)
           line)))

      result)))

(define-public dimension-arrows (arrow-stencil-maker #t #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ANNOTATIONS
;;
;; annotations are arrows indicating the numerical value of
;; spacing variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define*-public (annotate-y-interval layout name extent is-length
                                     #:key (color darkblue))
  (let ((text-props (cons '((font-size . -3)
                            (font-family . typewriter))
                          (layout-extract-page-properties layout)))
        (annotation #f))
    (define (center-stencil-on-extent stil)
      (ly:stencil-translate (ly:stencil-aligned-to stil Y CENTER)
                            (cons 0 (interval-center extent))))
    ;; do something sensible for 0,0 intervals.
    (set! extent (interval-widen extent 0.001))
    (if (not (interval-sane? extent))
        (set! annotation (interpret-markup
                          layout text-props
                          (make-simple-markup (simple-format #f "~a: NaN/inf" name))))
        (let ((text-stencil (interpret-markup
                             layout text-props
                             (make-whiteout-markup name)))
              (dim-stencil (interpret-markup
                            layout text-props
                            (make-whiteout-markup
                             (cond
                              ((interval-empty? extent)
                               "empty")
                              (is-length
                               (ice9-format #f "~,2f" (interval-length extent)))
                              (else
                               (ice9-format #f "(~,2f,~,2f)"
                                            (car extent) (cdr extent)))))))
              (arrows (ly:stencil-translate-axis
                       (dimension-arrows (cons 0 (interval-length extent)) 1.0)
                       (interval-start extent) Y)))
          (set! annotation
                (center-stencil-on-extent text-stencil))
          (set! annotation
                (ly:stencil-combine-at-edge arrows X RIGHT annotation 0.5))
          (set! annotation
                (ly:stencil-combine-at-edge annotation X LEFT
                                            (center-stencil-on-extent dim-stencil)
                                            0.5))
          (set! annotation
                (stencil-with-color annotation color))))
    annotation))


;; TODO: figure out how to annotate padding nicely
;; TODO: emphasize either padding or min-dist depending on which constraint was active
(define*-public (annotate-spacing-spec layout name spacing-spec
                                       start-Y-offset next-staff-Y
                                       #:key (base-color blue))
  (let* ((get-spacing-var (lambda (sym) (assoc-get sym spacing-spec 0.0)))
         (space (get-spacing-var 'basic-distance))
         (padding (get-spacing-var 'padding))
         (min-dist (get-spacing-var 'minimum-distance))
         (contrast-color (append (cdr base-color) (list (car base-color))))
         (min-dist-blocks (<= (- start-Y-offset min-dist) next-staff-Y))
         (min-dist-color (if min-dist-blocks contrast-color base-color))
         (name-string (if (string-null? name)
                          ""
                          (simple-format #f " (~a)" name)))
         (basic-annotation
          (annotate-y-interval layout
                               (simple-format #f "basic-dist~a" name-string)
                               (cons (- start-Y-offset space) start-Y-offset)
                               #t
                               #:color (map (lambda (x) (* x 0.25)) base-color)))
         (min-annotation
          (annotate-y-interval layout
                               (simple-format #f "min-dist~a" name-string)
                               (cons (- start-Y-offset min-dist) start-Y-offset)
                               #t
                               #:color min-dist-color))
         (extra-annotation
          (annotate-y-interval layout
                               (simple-format #f "extra dist~a" name-string)
                               (cons next-staff-Y (- start-Y-offset min-dist))
                               #t
                               #:color (map (lambda (x) (* x 0.5)) min-dist-color))))

    (stack-stencils X RIGHT 0.0
                    (list
                     basic-annotation
                     (if min-dist-blocks
                         min-annotation
                         (ly:stencil-add min-annotation extra-annotation))))))

(define-public (eps-file->stencil axis size file-name)
  (ly:note-extra-source-file file-name)
  (let*
      ((contents (ly:gulp-file file-name))
       (bbox (get-postscript-bbox (car (string-split contents #\nul))))
       (bbox-size (if (= axis X)
                      (- (list-ref bbox 2) (list-ref bbox 0))
                      (- (list-ref bbox 3) (list-ref bbox 1))
                      ))
       (factor (if (< 0 bbox-size)
                   (exact->inexact (/ size bbox-size))
                   0))
       (scaled-bbox
        (map (lambda (x) (* factor x)) bbox)))

    (if bbox
        (ly:make-stencil
         (list
          'eps-file
          file-name
          contents
          bbox
          factor)
         ;; Stencil starts at (0,0), since we have shifted the eps, and its
         ;; size is exactly the size of the scaled bounding box
         (cons 0 (- (list-ref scaled-bbox 2) (list-ref scaled-bbox 0)))
         (cons 0 (- (list-ref scaled-bbox 3) (list-ref scaled-bbox 1))))

        (ly:make-stencil "" '(0 . 0) '(0 . 0)))
    ))


(define-public (stencil-true-extent stencil axis)
  "Return the extent of the actual printed ink of @var{stencil}
on @var{axis}."
  ;; Simple strategy: compute skylines, take their max height.
  ;; If this becomes performance-critical, it can be rewritten
  ;; to inspect the stencil expressions and compute their extents
  ;; directly without building full skylines -- at the cost of
  ;; a lot more code.
  (let ((skyp (ly:skylines-for-stencil stencil (other-axis axis))))
    (cons (ly:skyline-max-height (car skyp))
          (ly:skyline-max-height (cdr skyp)))))
