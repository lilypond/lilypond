;;;; framework-scm.scm -- output full-page stencil expressions
;;;;
;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2004--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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


(define-module (scm framework-scm))

(use-modules
 (ice-9 regex)
 (ice-9 string-fun)
 (guile)
 (srfi srfi-1)
 (ice-9 pretty-print)
 (srfi srfi-13)
 (scm page)
 (lily))

(define format ergonomic-simple-format)

(define-public (output-framework basename book scopes fields)
  (let* ((file (open-output-file (format #f "~a.scm" basename))))

    (display ";;Creator: LilyPond\n" file)
    (display ";; raw SCM output\n" file)

    (for-each
     (lambda (page)
       (display ";;;;;;;;;;;;;;;;;;;;;;;;;;\n;;;PAGE\n" file)
       ;; The following two lines are alternates
       ;;(pretty-print (ly:stencil-expr page) file)
       (write (ly:stencil-expr page) file)
       )
     (map page-stencil (ly:paper-book-pages book)))))

(define-public output-classic-framework output-framework)

(define-public (convert-to-ps . args) #t)
(define-public (convert-to-pdf . args) #t)
(define-public (convert-to-png . args) #t)
