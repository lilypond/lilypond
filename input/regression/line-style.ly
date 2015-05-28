\version "2.19.21"

\header {
  texidoc = "Cover all line styles available."  
}

\relative {
  \override Glissando.breakable = ##t

  s2
  d''2 \glissando d'2
  \override Glissando.style = #'dashed-line
  d,2 \glissando d'2
  \override Glissando.style = #'dotted-line
  d,2 \glissando d'2
  
  \override Glissando.style = #'zigzag
  d,2 \glissando d'2
  \override Glissando.style = #'trill
  d,2 \glissando d'2
  
  \override Glissando.style = #'none
  d,2 \glissando d'2
}
