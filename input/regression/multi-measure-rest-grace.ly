\version "2.3.22"


\header {
texidoc= "Multi-measure rests are centered also in the case of grace notes."
}

\layout { raggedright = ##t }

<<
    \new Staff { R1 R1 R1*3 }
    \new Staff { \clef bass c1 \grace c8 c2 c2 c1  \grace c16 c2 c2 c1 }
>>
