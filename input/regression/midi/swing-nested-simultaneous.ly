\version "2.27.1"

\header {
  texidoc = "Swing functions like @code{\\applySwing}, @code{\\tripletFeel},
and @code{\\applySwingWithOffset} can be applied to simultaneous music
structures.  There should be no warning here."
}

#(ly:set-option 'warning-as-error)

\include "swing.ly"

\score {
  \tripletFeel 8
  {
    R1
    <<
      { c'8 d' e' f' g' a'4 b' }
      \\
      { c'4 e' g'2 }
    >>
  }
  \layout { }
  \midi { }
}
