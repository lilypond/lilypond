\version "2.23.7"

\header {
  texidoc = "
Comparison of rehearsalMarkFormatter functions.

The marks should read H, K, I, K, 93, XCVI, XCVJ, 8, AB, CC, Dd,
xcvi, xcvj, iij., boxed A, circled B, ovalled C, medium font D."
}

{
  %% Regression test rehearsal-mark-letter covers the default
  %% format-mark-letters (omit I)
  \set Score.rehearsalMarkFormatter = #format-mark-alphabet
  \mark 8 R1 \mark 11 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '(alphabet-omit-j))
  \mark 9 R \mark 10 R
  \set Score.rehearsalMarkFormatter = #format-mark-numbers
  \mark 93 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '(roman))
  \mark 96 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '(roman-ij))
  \mark 96 R
  \set Score.rehearsalMarkFormatter = #format-mark-barnumbers
  \mark 111 R
  % empty list (default values) = #format-mark-letters
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '())
  \mark 27 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '(repeat))
  \mark 28 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '(repeat mixedcase))
  \mark 29 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '(roman lowercase))
  \mark 96 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic
                                        '(roman-ij lowercase))
  \mark 96 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic
                                        '(roman-ij lowercase dot))
  \mark 3 R
  \set Score.rehearsalMarkFormatter = #format-mark-box-alphabet
  \mark 1 R
  \set Score.rehearsalMarkFormatter = #format-mark-circle-alphabet
  \mark 2 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '(oval))
  \mark 3 R
  \set Score.rehearsalMarkFormatter = #(format-mark-generic '(medium))
  \mark 4 R
}
