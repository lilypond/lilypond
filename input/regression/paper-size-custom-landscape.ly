\version "2.23.4"

\header {
  texidoc = "Setting a custom paper size (landscape)."
}

\book {
  \paper { #(set-paper-size '(cons (* 4 in) (* 70 bp)) 'landscape) }
  \score { c''2 }
}
