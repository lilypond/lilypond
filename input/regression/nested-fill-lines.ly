\version "2.16.0"

\header
{

  texidoc = "
Nested fill-lines should work properly.  In this example, both occurrences
of FOO should be centered.

"
  title = \markup \column {
    \fill-line { "|FOO|" }
    \fill-line { \fill-line { "|FOO|" } }
  }
}
\score {c''}
