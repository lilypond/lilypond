
\header
{

  texidoc = "With the @code{untied} music function, notes may be
  tagged as untied in chords."

}
\version "2.9.15"

\paper {
  ragged-right = ##t
}

\relative {
  <c e \untied g b> ~  < c e g b >
}
