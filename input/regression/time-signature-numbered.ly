\version "2.19.16"

\header {
  texidoc = "The numbered time signature style prints a fraction."
}

#(ly:expect-warning "strange time signature found: 1/99999")
#(ly:expect-warning "strange time signature found: 10/6")
#(ly:expect-warning "strange time signature found: 8/20")

\new Staff {
  \relative d' {
    \override Staff.TimeSignature.style = #'numbered
    \time 1/99999 d1*1/99999
    \time 8/20 d1*8/20
    \time 10/6 d1*10/6
    \time 4/4 d1
    \time 3/4 d2.
    \time 2/2 d1
    \time 99999/1 d1
  }
}
