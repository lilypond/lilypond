
\version "2.19.24"

\header{
  texidoc="
As a last resort, the placement of grobs can be adjusted manually, by
setting the @code{extra-offset} of a grob.
"
}

\layout{ ragged-right = ##t }


\relative c''{
  \applyOutput Bottom #(outputproperty-compatibility
		 (make-type-checker 'note-head-interface)
		 'extra-offset '(2 . 3))
  c2
  c
  \applyOutput Score #(outputproperty-compatibility (make-type-checker 'mark-interface) 'extra-offset '(-1 . 4))
  \mark A
  d1
  \mark \default
  e
}


