\version "2.19.21"

\header {
  texidoc="Woodwind diagram with partial fill and trills."
}

\markup {
  \woodwind-diagram
  #'tin-whistle
  #'((cc . (one1q two1h three3q four1qT five1qT3q sixT))
     (lh . ())
     (rh . ()))

  \override #'(woodwind-diagram-details . ((fill-angle . 90)))
  \woodwind-diagram
  #'tin-whistle
  #'((cc . (one1q two1h three3q four1qT five1qT3q sixT))
     (lh . ())
     (rh . ()))

  \raise #16 \column { one1q two1h three3q four1qT five1qT3q sixT }
}
