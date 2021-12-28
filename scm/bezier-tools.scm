;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010--2022 Carl D. Sorensen <c_sorensen@byu.edu>
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

(define (make-coord x-value y-value)
  "Make a coordinate pair from @var{x-valye} and @var{y-value}."
  (cons x-value y-value))

(define (coord+ coord1 coord2)
  "Add @var{coord1} to @var{coord2}, returning a coordinate."
  (cons (+ (car coord1) (car coord2))
        (+ (cdr coord1) (cdr coord2))))

(define (coord- coord1 coord2)
  "Subtract @var{coord2} from @var{coord1}."
  (cons (- (car coord1) (car coord2))
        (- (cdr coord1) (cdr coord2))))

(define (coord* scalar coord)
  "Multiply each component of @var{coord} by @var{scalar}."
  (cons (* (car coord) scalar)
        (* (cdr coord) scalar)))

(define (interpolated-control-points control-points split-value)
  "Interpolate @var{control-points} at @var{split-value}.  Return a
set of control points that is one degree less than @var{control-points}."
  (if (null? (cdr control-points))
      '()
      (let ((first (car control-points))
            (second (cadr control-points)))
        (cons* (coord+ first (coord* split-value (coord- second first)))
               (interpolated-control-points
                (cdr control-points)
                split-value)))))

(define (split-bezier bezier split-value)
  "Split a cubic bezier defined by @var{bezier} at the value
@var{split-value}.  @var{bezier} is a list of pairs; each pair is
is the coordinates of a control point.  Returns a list of beziers.
The first element is the LHS spline; the second
element is the RHS spline."
  (let* ((quad-points (interpolated-control-points
                       bezier
                       split-value))
         (lin-points (interpolated-control-points
                      quad-points
                      split-value))
         (const-point (interpolated-control-points
                       lin-points
                       split-value))
         (left-side (list (car bezier)
                          (car quad-points)
                          (car lin-points)
                          (car const-point)))
         (right-side (list (car const-point)
                           (list-ref lin-points 1)
                           (list-ref quad-points 2)
                           (list-ref bezier 3))))
    (cons left-side right-side)))
