\header {
  texidoc="Positioning of last ties is incorrect; ties between 2-1 and 1-1
  durations differ.  Direction is incorrect.";
}

\score {
  \context Staff <
    \context Voice = a \notes\relative c'' {
      \stemUp
      r2 a2~a1~a
    }
    \context Voice = y \notes\relative c' {
      \stemDown
      r2 a2~a1~a
    }
  >
  \paper {
    linewidth = 100 * \staffspace;
  }
}