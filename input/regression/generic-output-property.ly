\version "1.3.146"

\header{
texidoc="
As a last resort, the placement of grobs can be adjusted manually, by
setting the @code{extra-offset} of a grob.
"
}

\score{
	\notes\relative c''{
  	\outputproperty #(make-type-checker 'note-head-interface) 
		#'extra-offset = #'(2 . 3)
  	c2
	c
	\context Score {
		\outputproperty #(make-type-checker 'mark-interface) 
		#'extra-offset = #'(-1 . 4)
	}
	\mark A
	d1
	\mark \default
	e
}
\paper{
	linewidth=-1.0
}
}
