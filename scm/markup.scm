;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 2003--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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
  "The @code{markup} macro provides a LilyPond-like syntax for building
markups using Scheme keywords, replacing @code{\\@var{command}} with
@code{#:@var{command}}.  For example, this:

@example
\\markup @{ foo
          \\raise #0.2 \\hbracket \\bold bar
          \\override #'(baseline-skip . 4)
          \\bracket \\column @{ baz bazr bla @}
@}
@end example

@noindent
translates to this:

@example
(markup \"foo\"
        #:raise 0.2 #:hbracket #:bold \"bar\"
        #:override '(baseline-skip . 4)
        #:bracket #:column (\"baz\" \"bazr\" \"bla\"))
@end example
"

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
  "Convenience to make a new alist chain from @var{chain} by
prepending a binding of @var{key} to @var{val}.  This is similar to
@code{acons}, for alist chains (lists of alists)."
  `(((,key . ,val)) . ,chain))

(define-public (stack-stencil-line space stencils)
  "Adjoin a list of @var{stencils} along the x@tie{}axis, leaving
@var{space} between the end of each stencil and the beginning of the
following stencil.  Stencils with empty y@tie{}extent are not given
@var{space} before them and don't avoid overlapping other stencils."
  (stack-stencils X RIGHT space (filter ly:stencil? stencils)))

;;;; convert a full markup object to an approximate pure string representation

(define-public (markup-default-to-string-method layout props . args)
  "The default @code{markup->string} handler for markups, used when
@code{markup->string} encounters a markup that has no special
@code{as-string} expression defined.  This applies
@code{markup->string} on all markup arguments and joins the results,
separating them with spaces."
  (string-join
   ;; Don't add extra spaces for arguments not giving
   ;; a representation.
   (remove
    string-null?
    (map
     (lambda (arg)
       (markup->string arg #:layout layout #:props props))
     args))
   " "))

(define*-public (markup->string m #:key (layout #f) (props '()))
  "Convert a markup or markup list to an approximate string
representation.  This is useful for, e.g., PDF metadata and MIDI
markers.

The optional named @var{layout} and @var{props} argument are an output
definition and a property alist chain, like the ones that are used
when interpreting markups."
  (cond
   ((string? m)
    m)
   ((pair? m)
    (let ((first-elt (car m)))
      (cond
       ((or (markup-function? first-elt)
            (markup-list-function? first-elt))
        ;; m is a markup, or the application of a markup list command.
        ;; Look up the as-string handler of a command.
        (let ((handler (or (markup-function-as-string-method first-elt)
                           markup-default-to-string-method)))
          (apply handler layout props (cdr m))))
       ((markup-list? m)
        ;; A markup list that is not the result of a markup list
        ;; command.  This must be a list of markups or markup lists.
        ;; Join results by spaces.
        (apply markup-default-to-string-method layout props m))
       (else
        ;; Can occur if one argument to a markup function is a
        ;; list of anything.
        ""))))
   (else "")))
