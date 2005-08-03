
\header
{
  texidoc = "Broken tuplets are adorned with little arrows."
}
\version "2.7.4"

\paper {
  raggedright = ##t  
  indent = 0.0
}


\relative c'' {
  \set tupletNumberFormatFunction = #fraction-tuplet-formatter

  \override TupletBracket #'edge-text = #(cons
					  (markup #:fontsize 6
					     #:arrow-head 0 -1 #f)
					  (markup #:arrow-head 0 1 #f))
  \times 11/19 {
    c4 c4 c4 c4
    \bar "empty" \break
    c4 c4 c4 c4
    c4 c4 c4 c4
    \bar "empty" \break
    c4 c4 c4 c4
    c4 c4 c4 
  }
}   
