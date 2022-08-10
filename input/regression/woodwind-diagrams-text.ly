\version "2.19.21"

\header {
  texidoc="Woodwind diagrams with text."
}

\markup \column {
  \override #'(graphical . #f)
  \woodwind-diagram
  #'clarinet
  #'((cc . ())
     (lh . (ees))
     (rh . (one fis)))

  \override #'(font-size . -4)
  \override #'(graphical . #f)
  \woodwind-diagram
  #'clarinet
  #'((cc . ())
     (lh . (ees))
     (rh . (one fis)))

  \override #'(size . 0.5)
  \override #'(graphical . #f)
  \woodwind-diagram
  #'clarinet
  #'((cc . ())
     (lh . (ees))
     (rh . (one fis)))
}
