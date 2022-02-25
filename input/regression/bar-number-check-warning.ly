\version "2.19.21"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "Bar number is ~a; expected ~a") 3 15)

\header {

texidoc="@code{\\barNumberCheck} may be inserted to check whether the
current bar number is correct.  Checking is enabled by default for
layout and disabled by default for MIDI."

}

okMusic = \relative {
  c''1 |
  \barNumberCheck #2 % OK
  c1 |
}

\score {
  \relative {
    \okMusic
    \barNumberCheck #15 % Warning
    c''1
  }
}

\score {
  \midi { }

  \relative {
    \okMusic
    \barNumberCheck #34 % Wrong, but no warning for MIDI output
    c''1
  }
}
