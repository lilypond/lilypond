\version "2.21.0"

\header {
  texidoc = "
Comparison of markFormatter functions.

The marks should read H, J, H, K, I, K, 94, XCIV, 9, AB, BB, Bb, xciv,
boxed A, circled A, ovalled A, medium font A.
"
}

{
  % default: #format-mark-letters (omit I)
  \mark 8 R1 \mark 9 R
  \set Score.markFormatter = #format-mark-alphabet
  \mark 8 R \mark 11 R
  \set Score.markFormatter = #(format-mark-generic '(alphabet-omit-j))
  \mark 9 R \mark 10 R
  \set Score.markFormatter = #format-mark-numbers
  \mark 94 R
  \set Score.markFormatter = #(format-mark-generic '(roman))
  \mark 94 R
  \set Score.markFormatter = #format-mark-barnumbers
  \mark 111 R
  % empty list (default values) = #format-mark-letters
  \set Score.markFormatter = #(format-mark-generic '())
  \mark 27 R
  \set Score.markFormatter = #(format-mark-generic '(repeat))
  \mark 27 R
  \set Score.markFormatter = #(format-mark-generic '(repeat mixedcase))
  \mark 27 R
  \set Score.markFormatter = #(format-mark-generic '(roman lowercase))
  \mark 94 R
  \set Score.markFormatter = #format-mark-box-alphabet
  \mark 1 R
  \set Score.markFormatter = #format-mark-circle-alphabet
  \mark 1 R
  \set Score.markFormatter = #(format-mark-generic '(oval))
  \mark 1 R
  \set Score.markFormatter = #(format-mark-generic '(medium))
  \mark 1 R
}
