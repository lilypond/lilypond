\version "2.24.2"

\header {
  texidoc = "This is a regression test for an @code{\\autochange}
scenario reported in issue 6575.  The stem of the C should point down."
}

#(ly:set-option 'warning-as-error #t)

\new PianoStaff {
  \autoChange {
    r g c' d'
  }
}
