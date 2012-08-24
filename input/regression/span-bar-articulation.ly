\version "2.16.0"

\header {
  texidoc = "Articulations on cross-staff stems do not collide with
span bars."
}

\new GrandStaff <<
  \new Staff = "a" {g1 R s }
  \new Staff = "b" {
    \clef bass R1 r2. g8( b |
    d'\prall\espressivo \change Staff="a" b') g'2.}
>>
