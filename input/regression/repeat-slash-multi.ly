\version "2.19.21"

\header {
  texidoc = "Beat repeats for patterns containing identical durations
shorter than an eighth note use multiple slashes."
}

\relative {
  \repeat percent 2 { c'16 d e f }
  \repeat percent 4 { c32 e g e }
}
