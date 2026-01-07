\version "2.25.33"

\header {
  texidoc = "@code{\\resetTagGroup} can undo the effects of @code{\\tagGroup}."
}

\layout { ragged-right= ##t }

\markup headline = \markup \fontsize #4 \bold \pad-around #1 \etc

music = {
  \tag foo c'1^foo
  \tag bar d'1^bar
  \tag baz e'1^baz
  \tag test f'1^test
}

\score { \music }

\markup { \headline "Use “\\keepWithTag foo” for every following snippet." }
\score { \keepWithTag foo \music }

\markup { \headline "\\tagGroup foo,bar" }
\tagGroup foo,bar
\score { \keepWithTag foo \music }

\markup { \headline "\\resetTagGroups" }
\resetTagGroups
\score { \keepWithTag foo \music }

\markup {
  \headline \column {
    "tgI = \\tagGroupRef foo,test"
    "tgII = \\tagGroupRef bar,baz"
  }
}
tgI = \tagGroupRef foo,test
tgII = \tagGroupRef bar,baz
\score { \keepWithTag foo \music }

\markup { \headline "\\resetTagGroup \\tgII" }
\resetTagGroup \tgII
\score { \keepWithTag foo \music }

\markup { \headline "\\resetTagGroup \\tgI" }
\resetTagGroup \tgI
\score { \keepWithTag foo \music }
