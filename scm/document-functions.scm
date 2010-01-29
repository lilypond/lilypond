;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
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

(use-modules
 (ice-9 regex))

(define (dashify-underscores str)
   (regexp-substitute/global #f "_" str 'pre "-" 'post))

(define (format-c-header c-h)
  (regexp-substitute/global
   #f "," 
   (regexp-substitute/global #f "(SCM|\\)|\\() *" (dashify-underscores c-h)
			     'pre "" 'post)
   'pre " " 'post))

(define (document-scheme-function name c-header doc-string)
  (string-append
   "@defun " (symbol->string name)  " " (format-c-header c-header) "\n"
   doc-string
   "\n@end defun\n\n"))

(define all-scheme-functions
   (hash-fold
    (lambda (key val prior)
      (cons (cons key val)  prior))
    '() (ly:get-all-function-documentation)))

(define (all-scheme-functions-doc)
  (let* ((fdocs (map (lambda (x)
		       (document-scheme-function (car x) (cadr x) (cddr x)))
		     all-scheme-functions))
	 (sfdocs (sort fdocs ly:string-ci<?)))
    (make <texi-node>
      #:name "Scheme functions"
      #:desc "Primitive functions exported by LilyPond."
      #:text
      (apply string-append sfdocs))))

;; (dump-node (all-scheme-functions-doc)  (current-output-port) 0 )
