\version "2.21.6"

\header {
  texidoc = "Test for cross-staff beams.  Three issues are covered.  All stems,
  beams, and note heads should be positioned correctly and there should be no
  programming errors."
}

up = \change Staff = "up"
dn = \change Staff = "down"

% Issue 4182
\new PianoStaff <<
  \time 2/4
  \new Staff = "up"
  s2
  \new Staff = "down" <<
    {s8*3 s8 \p }
    { g'8[ \up e' \dn g' \up c'] }
  >>
>>

% Issue 4691
<<
  \new PianoStaff \autoChange { c'''16[ c''' c''' c] }
  \new Staff { s4\p }
>>

% Issue 6004
\new PianoStaff \transpose c c' <<
  \time 2/4
  \new Staff = "up"
  s2
  \new Staff = "down" <<
    { s8*3 s8 \p }
    { <g a>8[ \up <e f> \dn <g a> \up <c d>] }
  >>
>>
