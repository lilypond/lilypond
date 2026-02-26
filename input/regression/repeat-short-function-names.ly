\version "2.25.35"

\header{
  texidoc="@code{\\*} is an abbreviation for @code{\\repeat unfold} and
@code{\\%} is an abbreviation for @code{\\repeat percent}.  The output should
consist of one measure of quarter notes followed by three ``percent'' measures."
}

#(ly:set-option 'warning-as-error #t)

{
  \%4 \*4 c'4 \fine
}
