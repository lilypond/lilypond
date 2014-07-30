\version "2.19.11"

\header {
  texidoc = "Dot size and beamlet length should be scaled along
with notation size when using the @code{\magnifyStaff} command."
}

music = { a'8.[ a'16] }
{
  \magnifyStaff 0.50 \music \music \bar "|"
  \magnifyStaff 0.71 \music \bar "|"
  \magnifyStaff 1.00 \music \bar "|"
  \magnifyStaff 1.41 \music \bar "|"
  \magnifyStaff 2.00 \music \bar "|"
} \addlyrics {
  "0.50  " _ _ _
  "0.71  " _
  "1.00  " _
  "1.41  " _
  "2.00  " _
}
