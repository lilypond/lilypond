;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2023 David Kastrup <dak@gnu.org>
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

(use-modules (oop goops))

(define <Moment> (class-of (ly:make-moment 0)))
(define <Pitch> (class-of (ly:make-pitch 0 0)))
(define <Duration> (class-of (ly:make-duration 0)))

(define-method (+ (a <Moment>) (b <Moment>)) (ly:moment-add a b))
(define-method (- (a <Moment>) (b <Moment>)) (ly:moment-sub a b))
(define-method (- (a <Moment>)) (ly:moment-sub ZERO-MOMENT a))
;; Guile has a bug where it "optimises" (- x) to (- 0 x)
;; so we use an ugly workaround making the latter "legal"
;; Cf. https://debbugs.gnu.org/cgi/bugreport.cgi?bug=64508
(define-method (- (a <number>) (b <Moment>))
  (ly:moment-sub (ly:make-moment a) b))

;; We use <number> instead of separate rules for <integer> and
;; <fraction> here.  This also matches inexact numbers (floats) and
;; then leads to a type error for the underlying function rather than
;; not finding a suitable generic.

(define-method (* (a <Moment>) (b <number>)) (ly:moment-mul a b))
(define-method (* (a <number>) (b <Moment>)) (ly:moment-mul b a))
(define-method (/ (a <Moment>) (b <number>)) (ly:moment-div a b))
(define-method (/ (a <Moment>) (b <Moment>)) (ly:moment-main (ly:moment-div a b)))
(define-method (< (a <Moment>) (b <Moment>)) (ly:moment<? a b))

;; "compress" must be the weirdest name for stretching by a factor,
;; but that's what it does.

(define-method (* (a <Duration>) (b <number>)) (ly:duration-compress a b))
(define-method (* (a <number>) (b <Duration>)) (ly:duration-compress b a))
(define-method (/ (a <Duration>) (b <number>)) (ly:duration-compress a (/ b)))

;; Since "overaccidentals" are only considered at the typesetting
;; stage and not in intermediate results, pitch arithmetic is actually
;; commutative and associative.  Unless the two pitches are from
;; different scales but no code is really prepared for that case.

(define-method (+ (a <Pitch>) (b <Pitch>)) (ly:pitch-transpose a b))
(define-method (- (a <Pitch>) (b <Pitch>)) (ly:pitch-diff a b))
(define-method (- (a <Pitch>)) (ly:pitch-negate a))
;; Guile optimiser bug workaround for negation:
(define-method (- (a <number>) (b <Pitch>))
  (ly:pitch-diff (ly:make-pitch 0 a) b))
(define-method (< (a <Pitch>) (b <Pitch>)) (ly:pitch<? a b))
