\version "2.13.1"

#(set-default-paper-size "a6")

\header {
  texidoc = "The min-systems-per-page variable takes precedence over
the desire not to overfill a page."
}

\paper {
  min-systems-per-page = 20
}

\repeat unfold 21 { c'1 }