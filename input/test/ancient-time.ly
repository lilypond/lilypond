\version "1.5.68"
\header {
texidoc="Should use old style."
}

\score {
  \notes { 
    \property Staff.TimeSignature \override #'style = #'neo_mensural
    s1 
  }
}
