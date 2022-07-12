;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2011--2022 Neil Puttock <n.puttock@gmail.com>
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

(define (grob-property-path path)
  (string-join (map symbol->string path) " "))

(define (document-mod-list op)
  (let ((tag (car op))
        (name-sym (cadr op))
        (args (cddr op)))
    (case tag
      ((push)
       (let ((value (car args))
             (path (cdr args)))
         (string-append
          (format #f "@item Sets grob property @code{~a} "
                  (grob-property-path path))
          (format #f "in @code{@rinternals{~a}} to" name-sym)
          (if (pretty-printable? value)
              (format #f ":~a\n" (scm->texi value))
              (format #f " ~a.\n" (scm->texi value))))))
      ((pop)
       (string-append
        (format #f "@item Reverts grob property @code{~a} "
                (grob-property-path (car args)))
        (format #f "in @code{@rinternals{~a}}.\n"
                name-sym)))
      ((assign)
       (string-append
        (format #f "@item Sets translator property @code{~a} to" name-sym)
        (if (pretty-printable? (car args))
            (format #f ":~a\n" (scm->texi (car args)))
            (format #f " ~a.\n" (scm->texi (car args))))))
      ((unset)
       (format #f "@item Unsets translator property @code{~a}.\n"
               name-sym))
      ((consists)
       (format #f "@item Adds @code{@rinternals{~a}}.\n" name-sym))
      ((remove)
       (format #f "@item Removes @code{@rinternals{~a}}.\n" name-sym))
      (else ""))))

(define (document-context-mod context-mod-pair)
  (let* ((name-sym (car context-mod-pair))
         (mod-list (ly:get-context-mods (cdr context-mod-pair)))
         (docstring (filter (lambda (mod)
                              (eq? (car mod) 'description))
                            mod-list)))
    (format #f
            "@item @code{\\~a}
@funindex \\~a
~a
@itemize
~{~a ~}
@end itemize
"
            name-sym
            name-sym
            (if (pair? docstring)
                (cadar docstring)
                (begin
                  (ly:warning (G_ "context modification `~a' not documented.") name-sym)
                  "(undocumented; fixme)"))
            (map document-mod-list mod-list))))

(define (document-mod obj-pair)
  (and (ly:context-mod? (cdr obj-pair))
       (document-context-mod obj-pair)))

(define context-mods-doc-string
  (format #f
          "@table @asis
~a
@end table
"
          (string-join
           (filter-map
            document-mod
            (sort
             (ly:module->alist (current-module))
             identifier<?)))
          ""))
