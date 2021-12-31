\version "2.23.6"

\header {
  texidoc = "Balloons can be attached to glissandi."
}

\new Voice \with {
  \consists Balloon_engraver
}
{
  \balloonGrobText Glissando #'(0.5 . -2.5) glissando
  \override Glissando.bound-details.left.padding = 1.5
  \override Glissando.bound-details.right.padding = 1
  c1\glissando c''
}
