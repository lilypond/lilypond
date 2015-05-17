\header
{
  texidoc = "Formatting for isolated ties.

@itemize @bullet
@item short ties are in spaces
@item long ties cross staff lines
@item ties avoid flags of left stems.
@item ties avoid dots of left notes.

@item short ties are vertically centered in the space, as well those
that otherwise don't fit in a space

@item extremely short ties are put over the noteheads, instead of between.
 
@end itemize
"

}
\layout {
  indent = #0.0
  ragged-right = ##t
}

\version "2.19.21"

frag =
\relative {
  c''16 c2...~ 16 ~ 2... |
  c4~8 c8~16 c16~32 c16.~[ c64]~ 64[ c8..] |
}


\new Staff \with {
  \remove "Time_signature_engraver"
} {
  \stemUp

  \frag \break
  \transpose c d \frag\break
  \transpose c e \frag
  
}
