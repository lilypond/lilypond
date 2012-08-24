\version "2.16.0"

\header {
  texidoc = "Cross-staff stems avoid articulations.  Articulations that don't
get in the way of stems do not cause unwanted horizontal space.
"
}

\new GrandStaff <<
  \new Staff = "a" { s1 }
  \new Staff = "b" {
    \stemDown
    \clef bass
    d'8^\prall^\espressivo [\change Staff="a" g'' ]
    g'' [\change Staff="b" d'8^\prall^\espressivo ]
    \stemUp
    f, [\change Staff="a" b8_\prall_\espressivo ]
    b_\prall_\espressivo [\change Staff="b" f,8 ]
  }
>>

\new GrandStaff <<
  \new Staff = "a" { s1 }
  \new Staff = "b" {
    \stemDown
    \clef bass
    d'8 [\change Staff="a" g'' ]
    g'' [\change Staff="b" d'8 ]
    \stemUp
    f, [\change Staff="a" b8 ]
    b [\change Staff="b" f,8 ]
  }
>>
