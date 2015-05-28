\version "2.19.21"

\header {
  texidoc = "The numbered time signature style prints a fraction."
}

\new Staff {
  \relative {
    \override Staff.TimeSignature.style = #'numbered
    \time 4/4 d'1
    \time 3/4 d2.
    \time 2/2 d1
    \time 16/128 d8
    #(ly:expect-warning "strange time signature found: 10/6")
    \time 10/6 \tuplet 6/4 { d2. d2 d2. d2 }
  }
}
