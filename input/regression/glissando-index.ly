\version "2.19.21"

\header {
  texidoc = "Individual glissandi within a chord can be tweaked."
}

\relative {
  \once \override Voice.Glissando.style =
    #(lambda (grob)
       (if (= 1 (ly:grob-property grob 'glissando-index)) 'zigzag 'default))
  <d' f a>1 \glissando s1 <f a c>
}
