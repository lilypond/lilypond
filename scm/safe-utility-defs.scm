;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2020 Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <hanwen@xs4all.nl>
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
;;;
;;;      Author Ian Hulin
;;;      Date   16 October 2011
;;;

(define-module (scm safe-utility-defs)
  #:use-module (ice-9 optargs)
  #:export (safe-objects)
  #:export-syntax (define-safe-public)
  #:re-export-syntax (define*))

(if (string>? (version) "1.9.10")
    (use-modules (ice-9 curried-definitions)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Safe definitions utility

(define safe-objects
  (list))

(define (get-symbol arg)
  (if (pair? arg)
      (get-symbol (car arg))
      arg))


(define-macro (define-safe-public arglist . body)
  "Define a variable, export it, and mark it as safe, i.e. usable in
LilyPond safe mode.  The syntax is the same as `define*-public'."

  (let ((safe-symbol (get-symbol arglist)))
    `(begin
       (define* ,arglist
         ,@body)
       (set! safe-objects (cons (cons ',safe-symbol ,safe-symbol)
                                safe-objects))
       (export ,safe-symbol)
       ,safe-symbol)))
