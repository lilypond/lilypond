\version "2.25.35"

\header {
  texidoc = "Beat repeats for patterns containing identical durations
shorter than an eighth note use multiple slashes."
}

\relative {
  \%2 { c'16 d e f }
  \%4 { c32 e g e }
}
