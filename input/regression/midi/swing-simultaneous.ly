\version "2.27.1"

\header {
  texidoc = "Swing functions like @code{\\applySwing}, @code{\\tripletFeel},
and @code{\\applySwingWithOffset} can be applied to simultaneous music
structures.  There should be no warning here."
}

#(ly:set-option 'warning-as-error)

\include "swing.ly"

voice = \relative c' {
  \partial 4.
  \repeat volta 2 {
    { c8 d e | f g a4 b c | c8 d e }
  }
}

verseChords = \chordmode {
  \repeat volta 2 {
    { s4. f1 c4. }
  }
}


\score {
  {
    \applySwingWithOffset 8 #'(2 1) #(ly:make-moment 3/8)
    <<
      \chords { \verseChords }
      \voice
    >>
  }
  \layout { }
  \midi { }
}
