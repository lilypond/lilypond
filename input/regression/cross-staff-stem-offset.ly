\version "2.21.2"

\header {
  texidoc = "
  Cross-staff stems should not have their @code{X-offset} calculated too
  early because @code{direction} may not be known.  The last stem in this
  example should be on the left side of the note.
  "
}

up = \change Staff = "up"
dn = \change Staff = "down"

\new PianoStaff \transpose c c' <<
  \time 2/4
  \new Staff = "up"
  s2
  \new Staff = "down" <<
    {s8*3 s8 \p }
    { g8[ \up e \dn g \up c] }
  >>
>>
