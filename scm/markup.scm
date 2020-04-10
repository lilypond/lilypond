;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2003--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

(defmacro*-public markup (#:rest body)
  "The `markup' macro provides a lilypond-like syntax for building markups.

 - #:COMMAND is used instead of \\COMMAND
 - #:line ( ... ) is used instead of \\line { ... }
 - etc.

Example:
  \\markup { foo
            \\raise #0.2 \\hbracket \\bold bar
            \\override #'(baseline-skip . 4)
            \\bracket \\column { baz bazr bla }
  }
         <==>
  (markup \"foo\"
          #:raise 0.2 #:hbracket #:bold \"bar\"
          #:override '(baseline-skip . 4)
          #:bracket #:column (\"baz\" \"bazr\" \"bla\"))"

  (car (compile-all-markup-expressions `(#:line ,body))))

;; utility

(define (markup-join markups sep)
  "Return line-markup of MARKUPS, joining them with markup SEP"
  (if (pair? markups)
      (make-line-markup (list-insert-separator markups sep))
      empty-markup))


(define-public interpret-markup ly:text-interface::interpret-markup)

(define-public (interpret-markup-list layout props markup-list)
  (fold-right
   (lambda (m prev)
     (if (markup-command-list? m)
         (append (apply (car m) layout props (cdr m)) prev)
         (cons (interpret-markup layout props m) prev)))
   '()
   markup-list))

(define-public (prepend-alist-chain key val chain)
  (cons (acons key val (car chain)) (cdr chain)))

(define-public (stack-stencil-line space stencils)
  "Adjoin a list of @var{stencils} along the X axis, leaving
@var{space} between the end of each stencil and the beginning of the
following stencil.  Stencils with empty Y extent are not given
@var{space} before them and don't avoid overlapping other stencils."
  (stack-stencils X RIGHT space (filter ly:stencil? stencils)))

;;;; convert a full markup object to an approximate pure string representation

;; We ignore `page-ref-markup', because we don't want to get the
;; `gauge'- and `default'-string
;;
;; TODO:
;; - we would be interested in the computed result of `replace-markup' and
;;   `first-visible-markup', don't know how to get this, though
;;   For now all (not computed) arguments are caught.
;; - Other markup-commands to ignore?
(define markup-commands-to-ignore
  '(page-ref-markup))

(define-public (markup->string m . argscopes)
  (let* ((scopes (if (pair? argscopes) (car argscopes) '())))

    (define all-relevant-markup-commands
      ;; Returns a list containing the names of all markup-commands and
      ;; markup-list-commands with predicate @code{cheap-markup?} or
      ;; @code{markup-list?} in their @code{markup-command-signature}.
      ;; @code{table-of-contents} is not caught, same for user-defined commands.
      ;; markup-commands from @code{markup-commands-to-ignore} are removed.
      (lset-difference eq?
                       (map car
                            (filter
                             (lambda (x)
                               (let* ((predicates (markup-command-signature (cdr x))))
                                 (and predicates
                                      (not
                                       (null?
                                        (lset-intersection eq?
                                                           '(cheap-markup? markup-list?)
                                                           (map procedure-name predicates)))))))
                             (ly:module->alist (resolve-module '(lily)))))
                       markup-commands-to-ignore))

    ;; helper functions to handle string cons like string lists
    (define (markup-cons->string-cons c scopes)
      (if (not (pair? c)) (markup->string c scopes)
          (cons
           (markup->string (car c) scopes)
           (markup-cons->string-cons (cdr c) scopes))))
    (define (string-cons-join c)
      (if (not (pair? c)) c
          (string-join (list (car c) (string-cons-join (cdr c))) "")))

    ;; We let the following line in for future debugging
    ;; (display-scheme-music (sort all-relevant-markup-commands symbol<?))


    ;;;; Remark: below only works, if markup?- or markup-list? arguments are the
    ;;;;         last listed arguments in the commands definition
    ;;;; TODO: which other markup-(list)-commands should be special cased or
    ;;;;       completely excluded?
    (cond
     ((string? m) m)
     ((null? m) "")
     ((not (pair? m)) "")

     ;;;; special cases: \concat, \put-adjacent, \fill-with-pattern and
     ;;;;                \fromproperty-markup
     ;;;;
     ;;;; TODO do we really want a string-joined return-value for \concat and
     ;;;; \put-adjacent?
     ;;;; \overlay or \combine will return a string with spaces

     ;; handle \concat (string-join without spaces)
     ((and (pair? m) (equal? (car m) concat-markup))
      (string-cons-join (markup-cons->string-cons (cadr m) scopes)))

     ;; handle \put-adjacent (string-join without spaces)
     ((and (pair? m) (equal? (car m) put-adjacent-markup))
      (string-cons-join (markup-cons->string-cons (take-right m 2) scopes)))

     ;; handle \fill-with-pattern (ignore the filling markup)
     ((and (pair? m) (equal? (car m) fill-with-pattern-markup))
      (markup->string (take-right m 2) scopes))

     ;; fromproperty-markup reads property values from the header block:
     ((equal? (car m) fromproperty-markup)
      (let* ((varname (symbol->string (cadr m)))
             ;; cut off the header: prefix from the variable name:
             (newvarname (if (string-prefix? "header:" varname)
                             (substring varname 7)
                             varname))
             (var (string->symbol newvarname))
             (mod (make-module 1)))
        ;; Prevent loops by temporarily clearing the variable we have just looked up
        (module-define! mod var "")
        (markup->string (ly:modules-lookup scopes var) (cons mod scopes))))

     ((member (car m)
              (primitive-eval (cons 'list all-relevant-markup-commands)))
      (markup->string
       (if (> (length (last-pair m)) 1)
           (last-pair m)
           (car (last-pair m)))
       scopes))

     ;; ignore all other markup functions
     ((markup-function? (car m)) "")

     ;; handle markup lists
     ((list? m)
      (string-join (map (lambda (mm) (markup->string mm scopes)) m) " "))

     (else "ERROR, unable to extract string from markup"))))
