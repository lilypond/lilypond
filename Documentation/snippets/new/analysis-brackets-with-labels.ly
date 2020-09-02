\version "2.19.55"

\header {
  lsrtags = "editorial-annotations, tweaks-and-overrides"

  texidoc = "
Text may be added to analysis brackets through the @code{text} property
of the @code{HorizontalBracketText} grob.  Adding different texts to
brackets beginning at the same time requires the @code{\tweak} command.
Bracket text will be parenthesized after a line break.

"
  doctitle = "Analysis brackets with labels"
}

\layout {
  \context {
    \Voice
    \consists "Horizontal_bracket_engraver"
    \override HorizontalBracket.direction = #UP
  }
}

{
  \once\override HorizontalBracketText.text = "a"
    c''\startGroup d''\stopGroup
    \once\override HorizontalBracketText.text = "a'"
    e''\startGroup d''\stopGroup |
  c''-\tweak HorizontalBracketText.text
        \markup \bold \huge "b" \startGroup
     -\tweak HorizontalBracketText.text "a" \startGroup
    d''\stopGroup
    e''-\tweak HorizontalBracketText.text "a'" \startGroup
    d''\stopGroup\stopGroup |
  c''-\tweak HorizontalBracketText.text foo \startGroup
    d'' e'' f'' | \break
  g'' a'' b'' c'''\stopGroup
}
