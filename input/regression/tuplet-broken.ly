
\header
{

  texidoc = "Broken tuplets are adorned with little arrows. The arrows
  come from the @code{edge-text} property, and thus be replaced with
  larger glyphs or other text. "

}

\version "2.19.21"

\paper {
  ragged-right = ##t  
  indent = 0.0
}


\relative {
  \override TupletNumber.text = #tuplet-number::calc-fraction-text

  \override TupletBracket.edge-text = #(cons
					  (markup #:fontsize 6
					     #:arrow-head X LEFT #f)
					  (markup #:arrow-head X RIGHT #f))
  \tuplet 19/11 {
    c''4 c4 c4 c4
    \bar "" \break
    c4 c4 c4 c4
    c4 c4 c4 c4
    \bar "" \break
    c4 c4 c4 c4
    c4 c4 c4 
  }
}   
