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
