\header {
texidoc = "
Fret-diagrams may be printed left-handed
"
}

\version "2.19.21"

fretTst = {
  %% C major for guitar
  c'1 ^\markup { \fret-diagram "6-x;5-3-3;4-2-2;3-o;2-1;1-o;"}
}

\new Voice {
  \textLengthOn
  \fretTst
  \once \override TextScript.fret-diagram-details.handedness = #LEFT
  \fretTst
  \bar "||"
  \override TextScript.fret-diagram-details.orientation =
    #'landscape
  \fretTst
  \once \override TextScript.fret-diagram-details.handedness = #LEFT
  \fretTst
  \bar "||"
  \override TextScript.fret-diagram-details.orientation =
    #'opposing-landscape
  \fretTst
  \once \override TextScript.fret-diagram-details.handedness = #LEFT
  \fretTst
  \bar "|."
}
