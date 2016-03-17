\version "2.19.38"

\header {
  texidoc="
Noteheads do not extend above the upper staff line.
"
}

\new Voice \with {
  \override NoteHead.color = #green
  } {
  \relative {
    f'4 a c e \bar "|."
  }
}
