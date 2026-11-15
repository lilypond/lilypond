;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2015--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

;;; Helpers shared by several of the Scheme translators.

(define-public (ly:make-listener callback)
  "This is a compatibility wrapper for creating a @q{listener} for use
with @code{ly:add-listener} from a @var{callback} taking a single
argument.  Since listeners are equivalent to callbacks, this is no
longer needed."
  callback)

(define (has-one-or-less? lst) (or (null? lst) (null? (cdr lst))))

(define (has-at-least-two? lst) (not (has-one-or-less? lst)))

(define (all-equal? lst pred)
  (or (has-one-or-less? lst)
      (and (pred (car lst) (cadr lst)) (all-equal? (cdr lst) pred))))

(define-public (event-has-articulation? event-type stream-event)
  "Is @var{event-type} in the @code{articulations} list of the music causing
@var{stream-event}?"
  (if (ly:stream-event? stream-event)
      (any
       (lambda (art)
         (memq event-type (ly:music-property art 'types)))
       (ly:music-property (ly:event-property stream-event 'music-cause)
                          'articulations))
      #f))
