\version "2.23.4"

\header {
  texidoc = "@code{\vshape} works on cross-staff slurs."
}

\new PianoStaff <<
  \new Staff = "1" {
    \vshape #'((0 . 0) (0 . 0) (0 . 0) (0 . 0)) Slur
    c''8( 8
    \change Staff = "2"
    c''8 8)
  }
  \new Staff = "2" s2
>>
