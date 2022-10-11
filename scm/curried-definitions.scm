;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2022 Jean Abou Samra <jean@abou-samra.fr>
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


;; A replacement for Guile's (ice-9 curried-definitions) module that does
;; not lose docstrings, which matters for the Internals Reference.  This
;; can be replaced with standard (ice-9 curried-definitions) when we require
;; Guile 3.0.9 or later.  See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50068

(define-module (lily curried-definitions)
  #:replace ((cdefine . define)
             (cdefine* . define*)
             define-public
             define*-public))

(define-syntax make-currying-define
  (syntax-rules ::: ()
                ((_ currying-name lambda-name)
                 (define-syntax currying-name
                   (lambda (sintax)
                     (syntax-case sintax ()
                       ((_ ((head2 . rest2) . rest) docstring body body* ...)
                        (string? (syntax->datum #'docstring))
                        ;; Keep moving docstring to outermost lambda.
                        #'(currying-name (head2 . rest2)
                                         docstring
                                         (lambda-name rest body body* ...)))
                       ((_ (head . rest) body body* ...)
                        #'(currying-name head
                                         (lambda-name rest body body* ...)))
                       ((_ name val)
                        #'(define name val))))))))

(make-currying-define cdefine lambda)
(make-currying-define cdefine* lambda*)

(define-syntax make-currying-define-public
  (syntax-rules ::: ()
                ((_ public-name define-name)
                 (define-syntax public-name
                   (lambda (sintax)
                     (syntax-case sintax ()
                       ((_ binding body body* ...)
                        #`(begin
                            (define-name binding body body* ...)
                            (export #,(let find-name ((form #'binding))
                                        (syntax-case form ()
                                          ((head . tail)
                                           (find-name #'head))
                                          (name
                                           #'name))))))))))))

(make-currying-define-public define-public cdefine)
(make-currying-define-public define*-public cdefine*)
