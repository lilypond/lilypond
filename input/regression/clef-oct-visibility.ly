\version "2.14.0"

\header {

    texidoc = "Octavation signs may be added to clefs.  By default,
their break-visibility is derived from the associated clef, but it may
be overridden explicitly. The initial treble_8 clef should not have an
8, while the treble_8 clef after the tenor clef should.
These settings also need to apply to clefs on new lines."

}
\layout { ragged-right = ##t  }


\relative c' {
  \override Staff.OctavateEight #'break-visibility = #all-invisible

  \clef "treble_8"
  c2 c |
  c c | \break
  c \clef "tenor" c |
  \revert Staff.OctavateEight #'break-visibility
  \clef "treble_8"
  c2 c |
  c c | \break
  c c
}

