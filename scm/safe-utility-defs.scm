;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Jan Nieuwenhuizen <janneke@gnu.org>
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

(define-module (lily safe-utility-defs)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 optargs)
  #:export (safe-objects)
  #:export-syntax (define-safe-public)
  #:re-export-syntax (define*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Safe definitions utility

(define safe-objects
  (list))

(define-syntax define-safe-public
  (lambda (syntaks)
    "Define a variable, export it, and mark it as safe, i.e., usable in
LilyPond safe mode.  The syntax is the same as `define*-public'."
    (syntax-case syntaks ()
      ((_ binding form ...)
       (let ((name (let loop ((b #'binding))
                     (syntax-case b ()
                       ((head . rest) (loop #'head))
                       (x #'x)))))
         #`(begin
             (define*-public binding form ...)
             (set! safe-objects (acons '#,name #,name safe-objects))))))))
