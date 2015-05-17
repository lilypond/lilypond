\header {

    texidoc = "Hairpins extend to the extremes of the bound if there
    is no adjacent hairpin or dynamic-text.  If there is, the hairpin
    extends to the center of the column or the bound of the text
    respectively."

}
\version "2.19.21"


\layout { ragged-right = ##t }

\relative { 
  c''4 \< c4 \!
  c4 \< c \!\> c\!
  c4 \< c c \!  \fff\> c c\!
  
} 
