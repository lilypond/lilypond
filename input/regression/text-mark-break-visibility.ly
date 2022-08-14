\version "2.23.14"

\header {
  texidoc = "A text mark created by @code{\\textMark} is visible everywhere
except at the end of a line."
}

#(set-default-paper-size "a7")

\fixed c' {
  \textMark "Name chords"
  <c e g>2 <ees ges bes>
  \textMark "Write chords"
  s2_"D" s2_"Bm"
  \break
  \textMark "Write inversions"
  s2_"C 6" s2_"C 6/4"
}
