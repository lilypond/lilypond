\version "2.12.0"

\header
{
  texidoc = "The existence of a text mark does not affect the placement of a multimeasure rest."
}

\paper{ ragged-right=##t }

\relative g' {
    \mark \markup{ "foo foo foo foo foo foo"}
    R1 | g4 g g g |
}
