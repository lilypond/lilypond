\version "2.16.0"

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
    \mark "layout instruction"
    c c c c
  }
}

\score {
  \new DrumStaff {
    \mark "context def"
    c c c c
  }
  \layout { \context { \DrumStaff \unset clefGlyph } }
}

\score {
  \new DrumStaff \with { \unset clefGlyph } {
    \mark "context mod"
    c c c c
  }
}
