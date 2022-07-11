%%%% Table of contents functions.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2007--2022 Nicolas Sceaux <nicolas.sceaux@free.fr>
%%%% Copyright (C) 2020--2022 Valentin Villenave <valentin@villenave.net>
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

%% defined later, in a closure
#(define-public (add-toc-item! markup-symbol text)
  #f)
#(define-public (toc-items)
  #f)

%% TODO: this should be per-book, issue #4227

#(let (;; Maps TOC item IDs (symbols) to alists
       (toc-hashtab (make-hash-table))
       ;; Same, in alist form.  This is what we eventually want to return, but a
       ;; hash table avoids quadratic algorithms while constructing the TOC tree.
       (toc-alist '())
       ;; Map names, i.e. terminal symbols of the paths
       ;; (\tocItem foo.bar.baz ... has the name 'baz) to
       ;; TOC IDs.
       (toc-name-id-hashtab (make-hash-table)))
   (call-after-session (lambda ()
                         (hash-clear! toc-hashtab)
                         (set! toc-alist '())
                         (hash-clear! toc-name-id-hashtab)))
   (set! add-toc-item!
         (lambda* (markup-symbol text #:optional raw-path)
           (let* ((id (gensym "toc"))
                  (path (cond
                         ((symbol? raw-path) (list raw-path))
                         ;; Without a raw-path, we add an entry at the toplevel, which
                         ;; is the same as a one-element raw-path.
                         ((or (not raw-path) (null? raw-path)) (list id))
                         ((list? raw-path) raw-path)
                         (else (begin
                                (ly:warning (_i "Invalid toc label: ~a")
                                            raw-path))
                               (list id))))
                  (level
                   ;; Find which existing TOC entry, if any, to attach this entry to.
                   ;; The principle is that the first element of path is interpreted specially:
                   ;; it can refer to a previously defined nested node, as with
                   ;; \tocItem foo.bar "x"
                   ;; \tocItem bar.baz "y"
                   ;; This attaches bar as a subtree of foo, which can be handy in
                   ;; large nested TOCs. If there are several possibilities (foo.bar
                   ;; and baz.bar), we choose the one that added last.  This is
                   ;; achieved by simply overwriting any existing entry in
                   ;; toc-name-id-hashtab when doing the hashq-set!.
                   (match path
                     ((single)
                      (hashq-set! toc-name-id-hashtab single id)
                      0)
                     ((head . tail)
                      (let* ((node-id (hashq-ref toc-name-id-hashtab head))
                             (entry (and node-id (hashq-ref toc-hashtab node-id))))
                        (let loop ((path path)
                                   ;; entry corresponds to the entry for the first element
                                   ;; in the path.  path still contains its name so a warning
                                   ;; can be emitted if entry is #f.
                                   (entry entry)
                                   (level (and entry (1+ (assq-ref entry 'level)))))
                          (if entry
                              (let ((children (assq-ref entry 'children)))
                                (match path
                                  ((head name)
                                   ;; The last component is a newly created node.
                                   (hashq-set! children name id)
                                   (hashq-set! toc-name-id-hashtab name id)
                                   level)
                                  ((head . (and remaining (child . rest)))
                                   (loop remaining
                                         (let ((child-id (hashq-ref children child)))
                                           (and child-id (hashq-ref toc-hashtab child-id)))
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
                     (level . ,level))))
             ;; Register the new entry.
             (hashq-set! toc-hashtab id alist)
             (set! toc-alist (acons id alist toc-alist))
             (label id))))
   (set! toc-items (lambda ()
                     (reverse toc-alist))))

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
