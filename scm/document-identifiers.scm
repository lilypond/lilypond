;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2006--2022 Han-Wen Nienhuys <hanwen@lilypond.org>
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
       (full-doc (procedure-documentation func))
       (match-args (and full-doc (string-match "^\\([^)]*\\)\n" full-doc)))
       (arg-names (if match-args
                      (with-input-from-string (match:string match-args) read)
                      (circular-list "arg")))
       (doc (if match-args (match:suffix match-args) full-doc))
       (sign (ly:music-function-signature music-func))
       (type-names (map (lambda (pred)
                          (if (pair? pred)
                              (format #f "[~a]" (type-name (car pred)))
                              (format #f "(~a)" (type-name pred))))
                        sign))
       (signature-str
        (string-join
         (map (lambda (arg type) (format #f "@var{~a} ~a" arg type))
              arg-names (cdr type-names)))))
    (format #f
            "@item @code{~a~a} ~a ~a~a
@funindex ~a~a
~a
"
            (if (eq? (string-ref (symbol->string name-sym) 0) #\\) "" "\\")
            name-sym
            (car type-names)
            (if (string-null? signature-str) "" " - ")
            signature-str
            (if (eq? (string-ref (symbol->string name-sym) 0) #\\) "" "\\")
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
             identifier<?)))
          ""))
