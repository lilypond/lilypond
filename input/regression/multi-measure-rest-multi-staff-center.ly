\header { texidoc = "The centering of multi-measure rests is
independent on prefatory matter in other staves."

	  }
\version "2.3.22"

\score {
    << \new Staff  { R1 } 
     \new Staff { f'1  \clef bass } 

    >>

\layout { raggedright = ##t } 
}
