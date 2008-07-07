\header {
  texidoc = "Cover all line styles available"
  
}
\version "2.11.51"
\paper {
  ragged-right = ##T
}

\relative c'' {
  \override Glissando #'breakable = ##t

  s2
  d2 \glissando d'2
  \override Glissando #'style = #'dashed-line
  d,2 \glissando d'2
  \override Glissando #'style = #'dotted-line
  d,2 \glissando d'2

  \override Glissando #'style = #'zigzag
  d,2 \glissando d'2
  \override Glissando #'style = #'trill
  d,2 \glissando d'2
}
