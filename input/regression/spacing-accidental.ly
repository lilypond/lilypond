
\version "2.19.21"

\header {
  texidoc = "Accidentals sticking out to the left
of a note will take a little more space, but only if the spacing is tight." 
}

\layout { ragged-right = ##t}

\relative {
  \time 8/4
  c''2 c2 cis2 cis2 |
  c8 c8 cis8 cis8 cis c c c]
}





