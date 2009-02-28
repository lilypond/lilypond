\version "2.12.0"

%% defined later, in a closure
#(define-public (add-toc-item! markup-symbol text)
  #f)
#(define-public (toc-items)
  #f)

#(let ((toc-item-list (list)))
   (set! add-toc-item!
	 (lambda (markup-symbol text)
	   (let ((label (gensym "toc")))
	     (set! toc-item-list
		   (cons (list label markup-symbol text)
			 toc-item-list))
	     (make-music 'EventChord
	       'page-marker #t
	       'page-label label
	       'elements (list (make-music 'LabelEvent
				 'page-label label))))))
   (set! toc-items (lambda ()
		     (reverse toc-item-list))))

\paper {
  tocTitleMarkup = \markup \huge \column {
    \fill-line { \null "Table of Contents" \null }
    \hspace #1
  }
  tocItemMarkup = \markup \fill-line {
    \fromproperty #'toc:text \fromproperty #'toc:page
  }
}

#(define-markup-list-command (table-of-contents layout props) ()
  ( _i "Outputs the table of contents, using the paper variable
@code{tocTitleMarkup} for its title, then the list of lines
built using the @code{tocItem} music function
Usage: @code{\\markuplines \\table-of-contents}" )
  (cons (interpret-markup layout props
			  (ly:output-def-lookup layout 'tocTitleMarkup))
	(space-lines (chain-assoc-get 'baseline-skip props)
		    (map (lambda (toc-item)
			   (let ((label (car toc-item))
				 (toc-markup (cadr toc-item))
				 (text (caddr toc-item)))
			     (interpret-markup
			       layout
			       (cons (list (cons 'toc:page 
					    (markup #:page-ref label "XXX" "?"))
					   (cons 'toc:text text))
				     props)
			       (ly:output-def-lookup layout toc-markup))))
			 (toc-items)))))

tocItem = 
#(define-music-function (parser location text) (markup?)
   "Add a line to the table of content, using the @code{tocItemMarkup} paper
variable markup"
   (add-toc-item! 'tocItemMarkup text))
