
\version "2.16.0"
\header{
texidoc="
Beams can be typeset over fixed distance aligned staves, beam
beautification does not really work, but knees do. Beams should be
behave well, wherever the switching point is.
"
}

\context PianoStaff <<
  \new Staff = "one" \relative c'{
    \stemUp  c8[ c \change Staff=two \stemUp c c]
    c[ c c c]
    \change Staff=one
    \stemDown  f8[ f \change Staff=two \stemUp c c]
    r2
    \stemDown  c8[ c \change Staff=one \stemDown c c]
    r2
    \change Staff=two
    \stemUp  c8[ c \change Staff=one \stemDown f f]
    r2
  }
  \new Staff = "two" \relative c'{
    \clef bass
    s1
    s1
    s1
    s1
  }
>>


