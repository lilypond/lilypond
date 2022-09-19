\version "2.23.14"

\header{

  texidoc= "@code{\\unset} should be able to unset the
@samp{DrumStaff}-specific @samp{clefGlyph} equally well as layout
instruction, in a context definition, or as context modification.  All
systems here should revert to the @samp{Score}-level violin clef."

}

\layout { ragged-right = ##t }
\score {
  \new DrumStaff {
    \unset DrumStaff.clefGlyph
    \textMark "layout instruction"
    c c c c
  }
}

\score {
  \new DrumStaff {
    \textMark "context def"
    c c c c
  }
  \layout { \context { \DrumStaff \unset clefGlyph } }
}

\score {
  \new DrumStaff \with { \unset clefGlyph } {
    \textMark "context mod"
    c c c c
  }
}
