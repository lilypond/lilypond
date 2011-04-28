% FIXME: this is a fake version number to allow git master
% to compile.  See discussion on lilypond-devel.
\version "2.13.61"

\header {
  texidoc = "Individual glissandi within a chord can be tweaked."
}

\relative c' {
  \once \override Voice . Glissando #'style =
    #(lambda (grob)
       (if (eq? 1 (ly:grob-property grob 'glissando-index)) 'zigzag 'default))
  <d f a>1 \glissando s1 <f a c>
}
