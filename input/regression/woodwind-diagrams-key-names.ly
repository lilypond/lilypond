\version "2.19.21"

\header {
  texidoc="Woodwind diagrams for all instruments in woodwind-diagrams.scm
with key names, one pressed per text stencil."
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'piccolo
          #'((cc . ())
             (lh . (bes))
             (rh . (bes)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'flute
          #'((cc . ())
             (lh . (bes))
             (rh . (bes)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'flute-b-extension
          #'((cc . ())
             (lh . (bes))
             (rh . (bes)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'tin-whistle
          #'((cc . ())
             (lh . ())
             (rh . ()))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'oboe
          #'((cc . ())
             (lh . (I II b))
             (rh . (a)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'clarinet
          #'((cc . ())
             (lh . (a))
             (rh . (one fis)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'bass-clarinet
          #'((cc . ())
             (lh . (a))
             (rh . (one fis)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'low-bass-clarinet
          #'((cc . ())
             (lh . (a))
             (rh . (one fis)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'saxophone
          #'((cc . ())
             (lh . (ees))
             (rh . (e)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'soprano-saxophone
          #'((cc . ())
             (lh . (ees))
             (rh . (e)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'alto-saxophone
          #'((cc . ())
             (lh . (ees))
             (rh . (e)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'tenor-saxophone
          #'((cc . ())
             (lh . (ees))
             (rh . (e)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'baritone-saxophone
          #'((cc . ())
             (lh . (ees low-a))
             (rh . (e)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'bassoon
          #'((cc . ())
             (lh . (high-e low-b))
             (rh . (bes thumb-bes)))
      }
}

\relative {
  c'1^\markup
      \override #'(graphical . #f) {
        \woodwind-diagram
          #'contrabassoon
          #'((cc . ())
             (lh . (high-e low-b))
             (rh . (bes thumb-bes)))
      }
}
