\version "2.23.4"

\header {
  texidoc = "Setting a custom default paper size."
}

#(set-default-paper-size '(cons (* 100 mm) (* 3 cm)))

\book {
  \score { c''2 }
}
