\version "2.13.36"

\header {
  texidoc = "Stems with overridden 'length should not confuse height estimation.
This example should fit snugly on one page.
"
}

\paper {
  #(set-paper-size "a6")
  tagline = ##f
  system-system-spacing #'padding = #1.20
}

\new Voice {
  \voiceTwo
  \override Staff.Stem #'length = #0
  \repeat unfold 144 a
}
