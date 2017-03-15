\version "2.19.55"

\header {
  texidoc = "Labels may be added to analysis brackets through the
@code{text} property of the @code{HorizontalBracketText} object.  Use of
the @code{\tweak} command is necessary for assigning text uniquely to
brackets beginning at the same moment.  Text assignments reflect the
usual nesting order of brackets.
"
}

\layout {
  \context {
    \Voice
    \consists "Horizontal_bracket_engraver"
    \override HorizontalBracket.direction = #UP
  }
}

\relative c'' {
  \time 3/4
  \key f \major
  c4
  -\tweak HorizontalBracketText.text "contrasting period" \startGroup
  -\tweak HorizontalBracketText.text "a" \startGroup
  a8( bes c f)
  f4( e d)
  c d8( c bes c)
  \appoggiatura bes4 a2 g4\stopGroup
  \once\override HorizontalBracketText.text = "b"
  f'8 \startGroup
  r a, r d r
  c4( e, f)
  g8( bes) a4 g8( f)
  f2 \stopGroup \stopGroup r4
}
