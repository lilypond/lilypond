\version "2.25.25"

\header {
  texidoc="In staff groups where span bar lines are engraved, caesura
marks aligned on bar lines appear outside the extremal staves only.

In this case, the source code lists the bottom staff before the top and uses
@code{alignAboveContext} to control the layout.  A fermata should appear above
the bar line."
}

music = { R1 \caesura }

\new PianoStaff \with { caesuraType = #'((scripts . (fermata))) } <<
  \new Staff = "LH" \with { \clef "bass" } \music
  \new Staff = "RH" \with { alignAboveContext = "LH" } \music
>>
