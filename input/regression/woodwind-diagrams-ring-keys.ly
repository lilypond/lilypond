\version "2.23.11"

\header {
  texidoc="Woodwind diagram with ring key and ring trill."
}

\markup {
  \woodwind-diagram
  #'flute
  #'((cc . (one twoR threeRT))
     (lh . ())
     (rh . ()))

  \scale #'(2 . 2)
  \override #'(size . 0.5)
  \woodwind-diagram
  #'flute
  #'((cc . (one twoR threeRT))
     (lh . ())
     (rh . ()))
}
