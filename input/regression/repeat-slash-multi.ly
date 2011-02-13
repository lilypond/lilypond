\version "2.13.51"

\header {
  texidoc = "Beat repeats for patterns containing identical durations
shorter than an eighth note use multiple slashes."
}

\relative c' {
  \repeat percent 2 { c16 d e f }
  \repeat percent 4 { c32 e g e }
}
