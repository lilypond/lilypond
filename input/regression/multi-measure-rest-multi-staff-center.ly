\header { texidoc = "The centering of multi-measure rests is
independent on prefatory matter in other staves."

	  }
\version "2.1.7"

\score {
\notes    << \new Staff  { R1 } 
     \new Staff { r1  \clef bass } 

    >>

\paper { raggedright = ##t } 
}
