\version "2.27.5"

\header {
  texidoc = "
The argument lists of @code{\\override} and @code{\\revert} in context
modifications are parsed in the same manner (discarding any leading
context name) whether they are parsed directly or in the form of a
music expression (like it happens when preceded by @code{\\once}).

The output should color noteheads, stems, and beam in the first half
measure and revert to black afterwards."  }

\layout {
  \context {
    \Voice
    \override Beam.color = #blue
    \override Voice.NoteHead.color = #red
    \once \override Stem.color = #green
  }
}

{
  \*4 8
  \new Voice \with {
    \revert Beam.color
    \revert Voice.NoteHead.color
    \once \revert Stem.color } \*4 8
}
