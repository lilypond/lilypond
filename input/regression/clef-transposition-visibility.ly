\version "2.19.21"

\header {

    texidoc = "Clefs may be transposed.  By default, break-visibility
of ClefModifiers is derived from the associated clef, but it may
be overridden explicitly. The initial treble_8 clef should not have an
8, while the treble_8 clef after the tenor clef should.
These settings also need to apply to clefs on new lines."

}
\layout { ragged-right = ##t  }


\relative {
  \override Staff.ClefModifier.break-visibility = #all-invisible

  \clef "treble_8"
  c'2 c |
  c c | \break
  c \clef "tenor" c |
  \revert Staff.ClefModifier.break-visibility
  \clef "treble_8"
  c2 c |
  c c | \break
  c c
}

