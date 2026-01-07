\version "2.25.33"

\header {
  texidoc = "The commands @code{\\addToTagGroup} or
@code{\\removeFromTagGroup} can modify existing tag groups
defined by @code{\\tagGroupRef}."
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

\markup { \headline "tg = \\tagGroupRef foo,bar" }
tg = \tagGroupRef foo,bar
\score { \keepWithTag foo \music }

\markup { \headline "\\addToTagGroup \\tg baz" }
\addToTagGroup \tg baz
\score { \keepWithTag foo \music }

\markup { \headline "\\removeFromTagGroup \\tg bar" }
\removeFromTagGroup \tg bar
\score { \keepWithTag foo \music }

\markup { \headline "\\addToTagGroup \\tg test" }
\addToTagGroup \tg test
\score { \keepWithTag foo \music }

\markup { \headline "\\removeFromTagGroup \\tg foo,test" }
\removeFromTagGroup \tg foo,test
\score { \keepWithTag foo \music }
