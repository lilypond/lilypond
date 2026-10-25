%%%% Table of contents functions.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2007--2026 Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%% Copyright (C) 2020--2026 Valentin Villenave <valentin@villenave.net>
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

#(use-modules (ice-9 match))

#(define*-public (add-toc-item! markup-symbol text #:optional raw-path)
   ((car (create-toc!)) markup-symbol text raw-path))

#(define-public (toc-items)
   ((cdr (create-toc!))))

%% create-toc! checks whether $toc-handler in the current paper block contains
%% a workable pair of closures for adding and retrieving toc entries and
%% returns it; if necessary, creating it.  We need to go via the paper block
%% since that is what markup functions like \table-of-contents can work with.
%% If there is an optional key argument #:force #t, this will make sure that
%% the toc infrastructure is set up in the current paper block rather than any
%% of its ancestors.

#(define*-public (create-toc! #:key force)
   (define (toc-closure)
     (let (;; Maps TOC item IDs (symbols) to alists
           (toc-hashtab (make-hash-table))
           ;; Same, in alist form.  This is what we eventually want to return,
           ;; but a hash table avoids quadratic algorithms while constructing
           ;; the TOC tree.
           (toc-alist '())
           ;; Map names, i.e. terminal symbols of the paths (\tocItem
           ;; foo.bar.baz ... has the name 'baz) to TOC IDs.
           (toc-name-id-hashtab (make-hash-table)))
       (cons
        (lambda* (markup-symbol text #:optional raw-path)
          (let* ((id (gensym "toc"))
                 (path (cond
                        ((symbol? raw-path) (list raw-path))
                        ;; Without a raw-path, we add an entry at the
                        ;; toplevel, which is the same as a one-element
                        ;; raw-path.
                        ((or (not raw-path) (null? raw-path)) (list id))
                        ((list? raw-path) raw-path)
                        (else (begin
                                (ly:warning (_i "Invalid toc label: ~a")
                                            raw-path))
                              (list id))))
                 (parent #f) ; set below
                 (level
                  ;; Find which existing TOC entry, if any, to attach this
                  ;; entry to.  The principle is that the first element of
                  ;; path is interpreted specially: it can refer to a
                  ;; previously defined nested node, as with
                  ;;
                  ;;     \tocItem foo.bar "x"
                  ;;     \tocItem bar.baz "y"
                  ;;
                  ;; This attaches bar as a subtree of foo, which can be handy
                  ;; in large nested TOCs. If there are several possibilities
                  ;; (foo.bar and baz.bar), we choose the one that added last.
                  ;; This is achieved by simply overwriting any existing entry
                  ;; in toc-name-id-hashtab when doing the hashq-set!.
                  (match path
                    ((single)
                     (hashq-set! toc-name-id-hashtab single id)
                     0)
                    ((head . tail)
                     (let* ((root-id (hashq-ref toc-name-id-hashtab head)))
                       (let loop ((path path)
                                  ;; nested-id is the TOC ID of the first
                                  ;; element in the path.  path still contains
                                  ;; its name so a warning can be emitted if
                                  ;; nested-id is #f.
                                  (ancestor-id root-id)
                                  ;; Start with the level of the root, which
                                  ;; may be anywhere in the tree.
                                  (level (let ((entry (hashq-ref toc-hashtab root-id)))
                                           (and entry (1+ (assq-ref entry 'level))))))
                         (if ancestor-id
                             (let* ((entry (hashq-ref toc-hashtab ancestor-id))
                                    (children (assq-ref entry 'children)))
                               (match path
                                 ((head name)
                                  ;; The last component is a newly created
                                  ;; node.
                                  (hashq-set! children name id)
                                  (hashq-set! toc-name-id-hashtab name id)
                                  (set! parent ancestor-id)
                                  level)
                                 ((head . (and remaining (child . rest)))
                                  (loop remaining
                                        (hashq-ref children child)
                                        (1+ level)))))
                             (begin
                               (ly:warning (G_ "TOC node ~a not defined")
                                           (car path))
                               ;; Insert the node on the toplevel.
                               (let ((final-name (last path)))
                                 (hashq-set! toc-name-id-hashtab final-name id))
                               0)))))))
                 (alist
                  `((text . ,text)
                    (toc-markup . ,markup-symbol)
                    (children . ,(make-hash-table))
                    (level . ,level)
                    (parent . ,parent))))
            ;; Register the new entry.
            (hashq-set! toc-hashtab id alist)
            (set! toc-alist (acons id alist toc-alist))
            (label id)))
        (lambda () (reverse toc-alist)))))
   (cond ((and (not force)
               (ly:output-def-lookup
                (ly:parser-lookup '$defaultpaper) '$toc-handler))
          pair? => identity)
         (else
          (let ((tc (toc-closure)))
            (ly:output-def-set-variable!
             (ly:parser-lookup '$defaultpaper) '$toc-handler tc)
            tc))))

setupTableOfContents =
#(define-void-function () ()
   (_i "Set up a table of contents in the currently active paper block.

While a table of contents is established on the first call to @code{\\tocItem}
that cannot find one, using this function can establish a fresh table of
contents if one happens to already be accessible, or it can be used when the
first @code{\\tocItem} would establish one at the wrong layer.

Placing @code{\\setupTableOfContents} right before

@example
\\markuplist \\table-of-contents
@end example

will be an obvious choice when the table of contents precedes all referenced
items.
")
   (create-toc! #:force #t))

#(define* (format-toc-toplevel layout props arg #:optional)
  (_i "Print top level markups in bold.")
  (let ((proc (chain-assoc-get 'toc:toplevel-formatter props)))
    (interpret-markup layout props
     (if (zero? (chain-assoc-get 'toc:level props))
         (proc arg)
         arg))))

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
  "Print a table of contents.

This function uses the paper variable @code{tocTitleMarkup} for the title; it
then prints @code{\\tocItem} entries line by line.

@xref{Table of contents} for a complete discussion."
  (let ((titleMarkup (ly:output-def-lookup layout 'tocTitleMarkup))
        (indentMarkup (ly:output-def-lookup layout 'tocIndentMarkup))
        (toplevelFormatter (ly:output-def-lookup layout 'tocFormatMarkup))
        (toc-alist ((cdr (ly:output-def-lookup layout '$toc-handler
                                               ;; If you call the cdr of this
                                               ;; "default", you get an empty
                                               ;; list
                                               (cons #f list)))))
        (paper (let loop ((pap layout))
                 (cond ((ly:output-def-parent pap) ly:output-def? => loop)
                       (else pap)))))
    (ly:output-def-set-variable! paper 'label-alist-table
                                 (append (ly:output-def-lookup paper 'label-alist-table)
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
   (_i "Add @var{text} as an entry to the table of contents.

This uses the @code{tocItemMarkup} paper variable markup for formatting and
assigns it to @var{label} if one is provided.  If a hierarchy of labels is
given, make the current item a child of the corresponding objects.")
   (add-toc-item! 'tocItemMarkup text label))

newTocItem =
#(define-scheme-function (tocItemType) ((symbol? 'tocItemMarkup))
  (_i "Returns a variant of @code{\\tocItem} that creates a table of contents
entry for the table-of-content active at the time of its definition rather than
at the time of its call.  This can be useful if you want to maintain several
tables of contents simultaneously.

Called as @code{\\newTocItem \\default}, this is for the default
@code{'tocItemMarkup} category; instead of @code{\\default}, you can specify a
different category.

Normal use is right after the corresponding @code{\\setupTableOfContents}, for
example

@example
\\setupTableOfContents
topTocItem = \\newTocItem \\default
\\markuplist \\table-of-contents
@dots{}
@end example

after which @code{\\topTocItem} can be used for creating entries to that
specific table of contents.
")
  (let* ((paper (ly:parser-lookup '$defaultpaper))
         (clos (module-symbol-local-binding (ly:output-def-scope-module paper)
                                            '$toc-handler))
         (call (if (pair? clos) (car clos) add-toc-item!)))
    (if (not (pair? clos))
        (ly:input-warning (*location*) (G_ "No current table-of-contents")))
    (define-music-function (label text) ((symbol-list-or-symbol? '()) markup?)
      (_i "A special-purpose @code{\\tocItem} variant.")
      (call tocItemType text label))))
