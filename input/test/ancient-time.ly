\version "1.3.148"
\header {
texidoc="Should use old style."
}

\score {
  \notes { 
    \property Staff.TimeSignature \override #'style = #'oldC4/4
    \property Staff.TimeSignature \override #'font-family = #'ancient
    s1 
  }
}
