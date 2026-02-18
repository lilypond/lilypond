\version "2.23.4"

\header{
  texidoc="Repeat constructs without alternatives can be abbreviated using
@code{\\etc}.  The output should consist of one measure of quarter notes
followed by three ``percent'' measures."
}

#(ly:set-option 'warning-as-error #t)

testPercent = \repeat percent \etc
testQuad = \repeat unfold 4 \etc

{
  \testPercent 4 \testQuad c'4 \bar "|."
}
