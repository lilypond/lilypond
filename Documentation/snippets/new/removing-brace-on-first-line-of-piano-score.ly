\version "2.25.35"

\header {
  categories = "Keyboards, Staff notation, Tweaks and overrides"

  texidoc = "
This snippet removes the first brace from a @code{PianoStaff} or a
@code{GrandStaff}, together with the clefs. It may be useful when
cutting and pasting the engraved image into existing music.

The code uses @code{\\alterBroken} to hide the brace delimiter at the
beginning.
"

  doctitle = "Removing brace on first line of piano score"
} % begin verbatim


someMusic =  {
  \once \omit Staff.Clef
  \once \omit Staff.TimeSignature
  \*3 c1 \break
  \*5 c1 \break
  \*5 c1
}

\score {
  \new PianoStaff
  <<
    \new Staff = "right" \relative c'' \someMusic
    \new Staff = "left" \relative c' { \clef F \someMusic }
  >>
  \layout {
    indent=75\mm
    \context {
      \PianoStaff
      \alterBroken transparent #'(#t) SystemStartBrace
    }
  }
}
