;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2010 Han-Wen Nienhuys <hanwen@lilypond.org>
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

(use-modules (ice-9 format))

(define (document-music-function music-func-pair)
  (let*
      ((name-sym (car music-func-pair))
       (music-func (cdr music-func-pair))
       (func (ly:music-function-extract music-func))
       (arg-names
	(map symbol->string
	     (cddr (cadr (procedure-source func)))))
       (doc (procedure-documentation func))
       (sign (object-property func 'music-function-signature))
       (type-names (map type-name sign))

       (signature-str
	(string-join
	 (map (lambda (x) (format "@var{~a} (~a)"
				  (car x)
				  (cadr x)))
	      (zip arg-names type-names)))))
    (format
     "@item @code{~a}~a~a
@findex ~a
~a
"
     name-sym (if (equal? "" signature-str) "" " - ") signature-str
     name-sym
     (if doc doc "(undocumented; fixme)"))))


(define (document-object obj-pair)
  (cond
   ((ly:music-function? (cdr obj-pair))
    (document-music-function obj-pair))
   (else
    #f)))


(define (identifier<? a b)
  (ly:string-ci<?
   (symbol->string (car a))
   (symbol->string (car b))))


(define-public (identifiers-doc-string)
  (format
   "@table @asis
~a
@end table
"
   (string-join
    (filter
     identity
     (map
      document-object
      (sort
       (ly:module->alist (current-module))
       identifier<?)))
    "")))
