\version "2.25.23"

\header {
  texidoc = "Horizontal @code{Script} grobs are triggered by setting
@code{side-axis} to @code{X}.  This works with stand-alone notes and chords
regardless of whether other scripts, fingerings etc are already present.
The direction is taken from @code{scriptDefinitions} or from user settings.
For convenience the short-cuts @code{\atLeft} and @code{\atRight} take care
of setting @code{side-axis} and @code{direction}.
Collisions with dots and accidentals are avoided.
"
}

\layout {
  ragged-right = ##t
}

{
  \temporary \override Script.side-axis = #X
  b4.\prall <bes>4.\prall r4
  <b\prall>2. r4

  \bar "||"

  \temporary\override Script.direction = #LEFT
  b2\prall <bes>2\prall
  <b\prall>2 r2

  \bar "||"
  \break

  %% Extreme

  \set fingeringOrientations = #'(left)
  \set stringNumberOrientations = #'(left)
  \set strokeFingerOrientations = #'(left)
  <
   bes\tweak direction #LEFT \prall -1 \5
   f'!\tweak direction #RIGHT \mordent -2 \rightHandFinger #2
  >4

  \revert Script.side-axis
  \revert Script.direction

  \set stringNumberOrientations = #'(left up)
  \set strokeFingerOrientations = #'(up)

  \override StringNumber.X-align-on-main-noteheads = ##t
  \override StrokeFinger.X-align-on-main-noteheads = ##t
  <
   aes'
       -1
       -2
       \3
       \rightHandFinger #2
       \atLeft
       \prallprall
   bes'
       \atRight
       \mordent
       \tweak script-priority #0
       \4
       \rightHandFinger #3
  >2.
     \tweak script-priority #-50
     \arpeggio
     -3
     _> \upbow _\upmordent \turn
  \bar "|."
}
