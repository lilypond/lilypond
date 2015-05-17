\version "2.19.21"

\header {
  texidoc = "Glissandi begin after dots by default.  This behavior may
be changed by overriding the @code{start-at-dot} property.
"
}

\layout {
  indent = 0
  ragged-right = ##t
}

\relative {
  c''2.\glissando b4
  c2..\glissando b8
  c2...\glissando b16
  \bar "||"
  <c e g>2. -\tweak style #'trill \glissando <g b d>4
  \bar "||"
  c2...\glissando g''16
  \override Glissando.bound-details.left.start-at-dot = ##f
  c,,2...\glissando g''16
}
