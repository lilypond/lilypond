\version "2.14.0"

\header {
  lsrtags="winds"
  texidoc="
In may cases, the keys other than the central column can be
displayed by key name as well as by graphical means.
"

  doctitle = "Graphical and text woodwind diagrams"
}

\relative c'' {
  \textLengthOn
  c1^\markup
    \woodwind-diagram
      #'piccolo
      #'((cc . (one three))
         (lh . (gis))
         (rh . (ees)))

  c^\markup
    \override #'(graphical . #f) {
      \woodwind-diagram
        #'piccolo
        #'((cc . (one three))
           (lh . (gis))
           (rh . (ees)))
    }
}
