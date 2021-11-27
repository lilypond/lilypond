\version "2.23.6"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "Alternative bar numbering does not apply to repeats in
segno form.  These measures should be numbered 1 to@tie{}3."
}

\new Score \with {
  alternativeNumberingStyle = #'numbers-with-letters
  \override BarNumber.break-visibility = #end-of-line-invisible
  %% Omit these to sharpen the focus on the bar numbers.
  \omit CodaMark
  \omit JumpScript
} \fixed c' {
  \repeat segno 2 { f1 \alternative { g1 a1 } } | \barNumberCheck 4
}
