% FIXME: this is a fake version number to allow git master
% to compile.  See discussion on lilypond-devel.
\version "2.13.61"

\header {
  texidoc = "LilyPond typesets glissandi between chords."
}

\relative c' {
  c1 \glissando g'
  c,1 \glissando s1 g'
  <c, e>1 \glissando <g' b>
  <c, e>1 \glissando s1 <g' b>
  \set glissandoMap = #'((0 . 1) (1 . 0))
  <c, g'>1 \glissando s1 <d a'>
  \set glissandoMap = #'((0 . 0) (0 . 1) (0 . 2))
  c1 \glissando s1 <d f a>
  \set glissandoMap = #'((2 . 0) (1 . 0) (0 . 0))
  <d f a>1 \glissando s1 c
  \unset glissandoMap
  \once \override Voice . Glissando #'style =
    #(lambda (grob)
       (if (eq? 1 (ly:grob-property grob 'glissando-index)) 'zigzag 'default))
  <d f a>1 \glissando s1 <f a c>
}
