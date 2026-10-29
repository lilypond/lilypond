\version "2.27.5"

\header { texidoc="Setting @code{New_fingering_engraver}'s placement
options to @code{#f} gets a warning." }

\layout { ragged-right = ##t }

#(ly:set-option 'warning-as-error #t)

#(ly:expect-warning (G_ "no placement found for fingerings"))
#(ly:expect-warning (G_ "placing below"))
#(ly:expect-warning (G_ "no placement found for fingerings"))
#(ly:expect-warning (G_ "placing below"))
#(ly:expect-warning (G_ "no placement found for fingerings"))
#(ly:expect-warning (G_ "placing below"))

{
  \set fingeringOrientations = ##f
  \set stringNumberOrientations = ##f
  \set strokeFingerOrientations = ##f
  <c'-1\1\rightHandFinger #1 >1
}
