\version "2.19.55"

\header {
  texidoc = "Text is parenthesized when analysis brackets cross line
breaks.
"
}

\layout {
  \context {
    \Voice
    \consists "Horizontal_bracket_engraver"
  }
}

{
  c''
  -\tweak HorizontalBracketText.text \markup \draw-circle #1 #0.5 ##f
  \startGroup
  -\tweak HorizontalBracketText.text "a" \startGroup
  d'' e'' f''
  g'' a'' b'' c'''\stopGroup
  c'''-\tweak HorizontalBracketText.text "a'" \startGroup b'' a'' g''
  \break
  f'' e'' d'' c''\stopGroup\stopGroup
}
