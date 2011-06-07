\version "2.14.0"

\header
{

  texidoc = "
Nested fill-lines should work properly.  In this example, both occurences
of FOO should be centered.

"
  title = \markup \column {
    \fill-line { "|FOO|" }
    \fill-line { \fill-line { "|FOO|" } }
  }
}
\score {c''}
