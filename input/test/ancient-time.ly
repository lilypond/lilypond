\version "1.5.68"
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
