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

@item extremely short ties are put over the noteheads, instead of inbetween.
 
@end itemize
"

}
\layout {
  indent = #0.0
  ragged-right = ##t
}

\version "2.11.51"

frag =
\relative c'' {
  c16 c2...~ c16 ~ c2... |
  c4~c8 c8~c16 c16~c32 c16.~[ c64]~ c64[ c8..] |
}


\new Staff \with {
  \remove "Time_signature_engraver"
} {
  \stemUp

  \frag \break
  \transpose c d \frag\break
  \transpose c e \frag
  
}
