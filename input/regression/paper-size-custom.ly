\version "2.23.4"

\header {
  texidoc = "Setting a custom paper size."
}

\book {
  \paper { #(set-paper-size '(cons (* 4 in) (* 70 pt))) }
  \score { c''2 }
}

