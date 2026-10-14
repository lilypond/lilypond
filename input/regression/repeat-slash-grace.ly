\version "2.27.5"
\header {
  texidoc = "Beat repeats ignore grace in parallel music."
}

\layout { ragged-right = ##t }

\relative c'
<<
  \new Staff {
    \time 4/4
    \repeat "percent" 2 { c2 }

    %% the chairman dances
    \repeat "percent" 2 { g'8 g c, c }
    \repeat "percent" 4 { b8 b8 }
  }
  \new Staff {
    \*3 { c2 \acciaccatura { g'16 f e d }  c2 }
  }
>>
