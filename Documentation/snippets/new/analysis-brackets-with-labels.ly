\version "2.25.12"

\header {
  categories = "Contexts and engravers, Editorial annotations, Tweaks
                and overrides"

  texidoc = "
Text markup may be added to analysis brackets using the @code{text}
property of the @code{HorizontalBracketText} grob. Adding different
texts to brackets beginning at the same time requires the
@code{\\tweak} command.

Bracket text gets parenthesized after a line break.  The vertical
order of nested brackets can be controlled with the
@code{outside-staff-priority} property.
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
  c''-\tweak outside-staff-priority #801
      \tweak HorizontalBracketText.text
        \markup \bold \huge "b" \startGroup
     -\tweak HorizontalBracketText.text "a" \startGroup
    d''\stopGroup
    e''-\tweak HorizontalBracketText.text "a'" \startGroup
    d''\stopGroup\stopGroup |
  c''-\tweak HorizontalBracketText.text foo \startGroup
    d'' e'' f'' | \break
  g'' a'' b'' c'''\stopGroup
}
