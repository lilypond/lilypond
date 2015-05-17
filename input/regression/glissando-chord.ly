\version "2.19.21"

\header {
  texidoc = "LilyPond typesets glissandi between chords."
}

\relative {
  c'1 \glissando g'
  c,1 \glissando s1 g'
  <c, e>1 \glissando <g' b>
  <c, e>1 \glissando s1 <g' b>
  \set glissandoMap = #'((0 . 1) (1 . 0))
  <c, g'>1 \glissando s1 <d a'>
  \set glissandoMap = #'((0 . 0) (0 . 1) (0 . 2))
  c1 \glissando s1 <d f a>
  \set glissandoMap = #'((2 . 0) (1 . 0) (0 . 0))
  <d f a>1 \glissando s1 c
}
