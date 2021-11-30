\version "2.19.21"

\header {
  texidoc = "When a hairpin is broken, the broken parts should be open
             at the `breaking point'."
}

\layout {
  line-width = 4.\cm
}


\relative {
   c''1 \< \break
   c
   c\> \break
   c
   c\!
}

