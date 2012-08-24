\version "2.16.0"

\header {
  texidoc = "Individual glissandi within a chord can be tweaked."
}

\relative c' {
  \once \override Voice . Glissando #'style =
    #(lambda (grob)
       (if (eq? 1 (ly:grob-property grob 'glissando-index)) 'zigzag 'default))
  <d f a>1 \glissando s1 <f a c>
}
