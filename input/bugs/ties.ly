
\header {
  texidoc="Positioning of last ties is incorrect; ties between 2-1 and 1-1
  durations differ.";
}

\score {
  \context Staff <
    \context Voice = a \notes\relative c'' {
      \voiceOne
      r2 a2~a1~a
    }
    \context Voice = y \notes\relative c' {
      \voiceTwo
      r2 a2~a1~a
    }
  >
  \paper {
    linewidth = 100 * \staffspace;
  }
}
