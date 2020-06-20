%%%% Table of contents functions.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2007 Nicolas Sceaux <nicolas.sceaux@free.fr>,
%%%%               2020--2020 Valentin Villenave <valentin@villenave.net>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.21.2"

%% defined later, in a closure
#(define-public (add-toc-item! markup-symbol text)
  #f)
#(define-public (toc-items)
  #f)

#(define (assoc-prop-get val prop ls)
   (do ((ls ls (cdr ls)) (result '() result))
     ((null? ls) result)
     (if (and (car ls) (eq? val (assoc-get prop (cdar ls))))
         (set! result (cons (car ls) result)))))

#(define (assoc-name-get name ls)
   (assoc-prop-get name 'name ls))

#(define (check-parent-props me parent-entry big-alist)
   (let* ((my-id (car me))
          (my-alist (cdr me))
          (my-name (assoc-get 'name my-alist))
          (my-level (assoc-get 'level my-alist))
          (parent-alist (cdr parent-entry))
          (parent-id (car parent-entry))
          (parent-level (assoc-get 'level parent-alist))
          (siblings (assoc-get 'children parent-alist)))
     (assoc-set!
      big-alist parent-id
      (assoc-set! parent-alist 'children
                  (cons my-name (or siblings '()))))
     (if (< my-level (1+ parent-level))
         (assoc-set! big-alist my-id
                     (assoc-set! my-alist 'level
                                 (1+ parent-level))))))

#(define (setup-toc-hierarchy toc-ls)
   (map (lambda (entry)
          (let* ((parents (assoc-get 'parents (cdr entry)))
                 (direct-parent (if (null? parents) #f
                                    (car parents))))
            (if direct-parent
                (let ((found-parents
                       (assoc-name-get direct-parent toc-ls)))
                  (map (lambda (parent)
                         (check-parent-props
                          entry parent toc-ls))
                       found-parents)))))
        toc-ls)
   (reverse toc-ls))

#(let ((toc-item-list (list)))
   (call-after-session (lambda () (set! toc-item-list '())))
   (set! add-toc-item!
         (lambda* (markup-symbol text #:optional label)
           (let* ((id (gensym "toc"))
                  (path (cond
                         ((symbol? label) (list label))
                         ((or (not label) (null? label)) (list id))
                         ((list? label) (reverse label))
                         (else (begin
                                (ly:warning (_i "Invalid toc label: ~a")
                                            label))
                               (list id))))
                  (name (car path))
                  (parents (cdr path)))
             (set! toc-item-list
                   (acons id `((name . ,name)
                               (text . ,text)
                               (toc-markup . ,markup-symbol)
                               (parents . ,parents)
                               (children . ())
                               (level . ,(length parents)))
                          toc-item-list))
             #{ \label $id #})))
   (set! toc-items (lambda ()
                     (setup-toc-hierarchy toc-item-list))))

#(define* (format-toc-toplevel layout props arg #:optional only-if-multiple-levels?)
  (_i "Print top level markups in bold.")
  (let* ((proc (chain-assoc-get 'toc:toplevel-formatter props))
         (alist (ly:output-def-lookup layout 'label-alist-table))
         (levels (assoc-prop-get 1 'level alist)))
    (interpret-markup layout props
     (if (and
          (= 0 (chain-assoc-get 'toc:level props))
          (or (not only-if-multiple-levels?) (null? levels)))
         (proc arg) arg))))

\paper {
  tocTitleMarkup = \markup \huge \column {
    \fill-line { \null "Table of Contents" \null }
    \null
  }
  tocFormatMarkup = #make-bold-markup
  tocIndentMarkup = \markup \hspace #4
  tocItemMarkup = \markup \on-the-fly #format-toc-toplevel \fill-line {
    \line { \fromproperty #'toc:indent \fromproperty #'toc:text }
    \fromproperty #'toc:page
  }
}

tocItemWithDotsMarkup = \markup \fill-with-pattern #1 #RIGHT .
  \fromproperty #'toc:text \fromproperty #'toc:page

#(define-markup-list-command (table-of-contents layout props) ()
  #:properties ((baseline-skip))
  ( _i "Outputs the table of contents, using the paper variable
@code{tocTitleMarkup} for its title, then the list of lines
built using the @code{tocItem} music function.
Usage: @code{\\markuplist \\table-of-contents}" )
  (let ((titleMarkup (ly:output-def-lookup layout 'tocTitleMarkup))
        (indentMarkup (ly:output-def-lookup layout 'tocIndentMarkup))
        (toplevelFormatter (ly:output-def-lookup layout 'tocFormatMarkup))
        (toc-alist (toc-items)))
    (ly:output-def-set-variable! layout 'label-alist-table
                                 (append (ly:output-def-lookup layout 'label-alist-table)
                                         toc-alist))
    (cons (interpret-markup layout props titleMarkup)
          (space-lines baseline-skip
                       (map (lambda (toc-item)
                              (let* ((label (car toc-item))
                                     (alist (cdr toc-item))
                                     (toc-markup (assoc-get 'toc-markup alist))
                                     (text (assoc-get 'text alist))
                                     (level (assoc-get 'level alist)))
                                (interpret-markup
                                 layout
                                 (cons (list
                                        (cons 'toc:page
                                              (markup #:with-link label
                                                      #:page-ref label "XXX" "?"))
                                        (cons 'toc:text (markup #:with-link label text))
                                        (cons 'toc:label label)
                                        (cons 'toc:level level)
                                        (cons 'toc:toplevel-formatter toplevelFormatter)
                                        (cons 'toc:indent
                                              (make-line-markup
                                               (make-list level indentMarkup))))
                                       props)
                                 (ly:output-def-lookup layout toc-markup))))
                            toc-alist)))))

tocItem =
#(define-music-function (label text) ((symbol-list-or-symbol? '()) markup?)
   (_i "Add a line to the table of contents, using the @code{tocItemMarkup}
paper variable markup and assigning it to @var{label} if one is provided.
If a hierarchy of labels is given, make the current item a child of
the corresponding objects.")
   (add-toc-item! 'tocItemMarkup text label))
