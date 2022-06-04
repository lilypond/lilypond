;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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


(define (doc-markup-function-properties func)
  (let ((properties (markup-function-properties func))
        (prop-strings (list)))
    (for-each (lambda (prop-spec)
                (set! prop-strings
                      (if (list? prop-spec)
                          ;; either (prop) or (prop value)
                          (cons (if (null? (cdr prop-spec))
                                    (format #f "@item @code{~a}\n" (car prop-spec))
                                    (format #f "@item @code{~a} (@code{~s})\n"
                                            (car prop-spec)
                                            (cadr prop-spec)))
                                prop-strings)
                          ;; a markup command: get its properties
                          ;; FIXME: avoid cyclical references
                          (append (doc-markup-function-properties prop-spec)
                                  prop-strings))))
              (or properties (list)))
    prop-strings))

(define (doc-markup-function func-pair)
  (let* ((f-name (symbol->string (car func-pair)))
         (func (cdr func-pair))
         (full-doc (procedure-documentation func))
         (match-args (and full-doc (string-match "^\\([^)]*\\)\n" full-doc)))
         (arg-names (if match-args
                        (with-input-from-string (match:string match-args) read)
                        (circular-list "arg")))
         (doc-str (if match-args (match:suffix match-args) full-doc))
         (sig (markup-command-signature func))
         (sig-type-names (map type-name sig))
         (signature-str
          (string-join
           (map (lambda (x y)
                  (format #f "@var{~a} (~a)" x y))
                arg-names  sig-type-names)
           " " )))

    (string-append
     "\n\n@item @code{\\" f-name "} " signature-str
     "\n@funindex \\" f-name "\n"
     (if (string? doc-str)
         doc-str
         "")
     (let ((prop-strings (doc-markup-function-properties func)))
       (if (null? prop-strings)
           "\n"
           (string-append "\n\n\nUsed properties:\n@itemize\n"
                          (string-concatenate prop-strings)
                          "@end itemize\n"))))))

(define (markup-name<? a b)
  (ly:string-ci<? (symbol->string (car a)) (symbol->string (car b))))

(define all-markup-commands '())
(define all-markup-list-commands '())

(for-each
 (lambda (m)
   (module-for-each (lambda (name var)
                      (let* ((str-name (symbol->string name))
                             (fixed-str-name
                              (regexp-substitute/global
                               #f "-markup(-list)?" str-name 'pre "" 'post))
                             (fixed-name (string->symbol fixed-str-name))
                             (val (variable-ref var)))
                        (cond ((markup-function? val)
                               (set! all-markup-commands
                                     (acons fixed-name val all-markup-commands)))
                              ((markup-list-function? val)
                               (set! all-markup-list-commands
                                     (acons fixed-name val all-markup-list-commands))))))
                    (module-public-interface m)))
 (cons (current-module) (map resolve-module '((lily) (lily accreg)))))

(set! all-markup-commands (sort! all-markup-commands markup-name<?))
(set! all-markup-list-commands (sort! all-markup-list-commands markup-name<?))

(define (markup-category-doc-node category)
  (let* ((category-string (symbol->string category))
         (category-name (string-capitalize
                         (regexp-substitute/global
                          #f "-" category-string 'pre " " 'post)))
         (markup-functions (filter
                            (lambda (fun)
                              (let ((cats (markup-function-category (cdr fun))))
                                (if (pair? cats)
                                    (memq category cats)
                                    (eq? category cats))))
                            all-markup-commands)))

    (make <texi-node>
      #:appendix #t
      #:name category-name
      #:desc ""
      #:text (string-append
              "@table @asis"
              (string-concatenate
               (map doc-markup-function markup-functions))
              "\n@end table"))))

(define (markup-doc-node)
  (make <texi-node>
    #:appendix #t
    #:name "Text markup commands"
    #:desc ""
    #:text "The following commands can all be used inside @code{\\markup @{ @}}."
    #:children (let* (;; when a new category is defined, update `ordered-categories'
                      (ordered-categories '(font
                                            align
                                            graphic
                                            music
                                            conditionals
                                            instrument-specific-markup
                                            accordion-registers
                                            other))
                      (raw-categories
                       (fold (lambda (next union)
                               (let ((cat (markup-function-category next)))
                                 (cond ((pair? cat)
                                        (lset-union eq? cat union))
                                       ((symbol? cat)
                                        (lset-adjoin eq? cat union))
                                       (else union))))
                             '()
                             all-markup-commands))
                      (categories (append ordered-categories
                                          (sort (lset-difference eq?
                                                                 raw-categories
                                                                 ordered-categories)
                                                symbol<?))))
                 (map markup-category-doc-node categories))))

(define (markup-list-doc-string)
  (string-append
   "@table @asis"
   (string-concatenate
    (map doc-markup-function all-markup-list-commands))
   "\n@end table"))
