\version "2.17.2"

\header {
  texidoc = "Dynamics do not horizontally shift when attached to
an axis-group extremal cross staff grob that's extremal side
(UP or DOWN) is the same as its direction.
"
}

\new PianoStaff <<
  \new Staff = "up" {
    s1 |
  }
  \new Staff = "down" {
    \clef bass
    \stemDown
    % keep staff alive
    <c,, c,>8 [ <c,, c,>8_\f
    \change Staff = "up"
    g' g' ]
    r2 |
  }
>>
