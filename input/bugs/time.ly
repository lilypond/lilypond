\version "1.3.148"
\header {
texidoc="Should use old style."
}

\score {
  \notes { 
    \property Staff.TimeSignature \override #'style = #'oldC4/4
    s1 
  }
}
