\version "2.19.11"

\header {
  texidoc = "Staff line thickness should be scaled along with
staff size when using the @code{\magnifyStaff} command.  Staff
lines can get thicker than the default, but not thinner."
}

{
  \magnifyStaff 0.50 b'1
  \magnifyStaff 0.71 b'1
  \magnifyStaff 1.00 b'1
  \magnifyStaff 1.41 b'1
  \magnifyStaff 2.00 b'1
} \addlyrics {
  " 0.50 "
  " 0.71 "
  " 1.00 "
  " 1.41 "
  " 2.00 "
}
