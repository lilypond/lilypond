\version "2.23.14"

#(ly:expect-warning "unterminated tie")

\header {
  texidoc = "A warning gets printed when there's an unterminated
tie at the end of input.
"
}

{ b2 ~ b ~ }

