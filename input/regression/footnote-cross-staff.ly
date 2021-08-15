\version "2.23.4"

\header {
  texidoc = "Footnotes work on cross-staff grobs."
}

\paper {
  #(set-paper-size "a7landscape")
}

\book {
  \new PianoStaff <<
    \new Staff = "1" {
      \footnote #'(-1 . 0.5) "marcato" Script
      c''8\tweak padding 1 _^ 8
      \change Staff = "2"
      c''8 8
    }
    \new Staff = "2" s2
  >>
}
