
\header {

  texidoc = "With @code{Shape_note_heads_engraver}, the style of the
note head is adjusted according to the step of the scale, as measured
relative to the @code{tonic} property."

}
\version "2.5.1"

fragment = {
  \key c \major
  \set shapeNoteStyles = ##(triangle cross slash  triangle cross slash  triangle cross slash)
  c d e f g a b c 
  \set shapeNoteStyles = ##(do re mi fa #f la ti)
  b a g f e d c 
}


\transpose c d 
\new Voice \with {
  \remove "Note_heads_engraver"
  \consists "Shape_note_heads_engraver"
} \relative {

  \fragment
}
   
