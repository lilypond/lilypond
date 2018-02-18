\version "2.21.0"

\header {
  texidoc = "
Comparison of markFormatter functions.

The marks should read H, K, I, K, 93, XCIV, 7, AB, CC, Dd, xcvi,
boxed A, circled B, ovalled C, medium font D.
"
}

{
  %% Regression test rehearsal-mark-letter covers the default
  %% format-mark-letters (omit I)
  \set Score.markFormatter = #format-mark-alphabet
  \mark 8 R1 \mark 11 R
  \set Score.markFormatter = #(format-mark-generic '(alphabet-omit-j))
  \mark 9 R \mark 10 R
  \set Score.markFormatter = #format-mark-numbers
  \mark 93 R
  \set Score.markFormatter = #(format-mark-generic '(roman))
  \mark 94 R
  \set Score.markFormatter = #format-mark-barnumbers
  \mark 111 R
  % empty list (default values) = #format-mark-letters
  \set Score.markFormatter = #(format-mark-generic '())
  \mark 27 R
  \set Score.markFormatter = #(format-mark-generic '(repeat))
  \mark 28 R
  \set Score.markFormatter = #(format-mark-generic '(repeat mixedcase))
  \mark 29 R
  \set Score.markFormatter = #(format-mark-generic '(roman lowercase))
  \mark 96 R
  \set Score.markFormatter = #format-mark-box-alphabet
  \mark 1 R
  \set Score.markFormatter = #format-mark-circle-alphabet
  \mark 2 R
  \set Score.markFormatter = #(format-mark-generic '(oval))
  \mark 3 R
  \set Score.markFormatter = #(format-mark-generic '(medium))
  \mark 4 R
}
