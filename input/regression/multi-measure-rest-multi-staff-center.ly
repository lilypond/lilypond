\header { texidoc = "The centering of multi-measure rests is
independent on prefatory matter in other staves."

	  }
\version "2.2.0"

\score {
\notes    << \new Staff  { R1 } 
     \new Staff { f'1  \clef bass } 

    >>

\paper { raggedright = ##t } 
}
