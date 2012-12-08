\header {

  texidoc = "With @code{shapeNoteStyles}, the style of the note head
is adjusted according to the step of the scale, as measured relative
to the @code{tonic} property."

}
\version "2.16.0"

fragment = {
  \key c \major
  \set shapeNoteStyles = ##(do re mi fa sol la ti)
  c1 d e f g a b c d e f g a b c
  c,,2 d e f g a b c d e f g a b c
  c,,4 d e f g a b c d e f g a b c
}

\transpose c d {
  \relative c' {
    \fragment
  }
}
