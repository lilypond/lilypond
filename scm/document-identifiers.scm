;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2023 Han-Wen Nienhuys <hanwen@lilypond.org>
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
       (prefix (if (eqv? (string-ref (symbol->string name-sym) 0) #\\) "" "\\"))
       (music-func (cdr music-func-pair))
       (func (ly:music-function-extract music-func))
       (doc (procedure-documentation func))
       (arg-names (syntax-function-procedure-arguments func))
       (signature (ly:music-function-signature music-func))
       (signature-str
        (string-join (map (lambda (arg sign)
                            (if (pair? sign)
                                (format #f "[@var{~a} (~a)]"
                                        arg (type-name (car sign)))
                                (format #f "@var{~a} (~a)"
                                        arg (type-name sign))))
                          arg-names (cdr signature)))))
    (format #f
            "@item @code{~a~a} ~a @result{} ~a
@funindex ~a~a
~a
"
            prefix
            name-sym
            signature-str
            (type-name (if (pair? (car signature))
                           (caar signature)
                           (car signature)))

            prefix
            name-sym

            (if (and doc (not (string-null? doc)))
                doc
                (begin
                  (ly:warning (G_ "music function `~a' not documented.") name-sym)
                  "(undocumented; fixme)")))))


(define (document-object obj-pair)
  (and (ly:music-function? (cdr obj-pair))
       (document-music-function obj-pair)))

(define-public (identifiers-doc-string)
  (format #f
          "@table @asis
~a
@end table
"
          (string-join
           (filter-map
            document-object
            (sort
             (ly:module->alist (current-module))
             identifier<?))
           "\n")
          ""))
