;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2010 Ian Hulin <ian@hulin.org.uk>
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

(define-module (scm guile-debugger)
  #:use-module (ice-9 debugger)
  #:use-module (ice-9 debugging traps)
  #:use-module (ice-9 debugging trace)
  #:use-module (ice-9 debugging steps)
  #:use-module (ice-9 debugging ice-9-debugger-extensions)
  #:use-module (ice-9 readline)
  #:export (set-break! set-trace! set-trace-subtree!))

(define (set-break! proc)
  (install-trap (make <procedure-trap>
		      #:procedure proc
		      #:behaviour debug-trap)))

(define (set-trace! proc)
  (install-trap (make <procedure-trap>
		      #:procedure proc
		      #:behaviour (list trace-trap
					trace-at-exit))))

(define (set-trace-subtree! proc)
  (install-trap (make <procedure-trap>
		      #:procedure proc
		      #:behaviour (list trace-trap
					trace-until-exit))))
