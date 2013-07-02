
\header {

  texidoc = "If @code{tupletFullLength} is set, tuplets end at the
start of the next non-tuplet note.  "

}
\version "2.17.11"

\paper { ragged-right = ##t
  indent = 0.0
}

\relative c'' \new Voice \with {
  \remove  Forbid_line_break_engraver
  \override Beam.breakable = ##t
}
{
  \set tupletFullLength = ##t
  c4
  \tuplet 3/2 { c8[ c c] }
  \tuplet 3/2 { c8[ c \bar "-" \break c] }
  << \tuplet 3/2 { c8[ c c]  }
     { s4*5/6 \bar "-" \break } >>
  c4
  \tuplet 3/2 { c8[ c c] }
  
  \bar "|." \key c \minor
}
