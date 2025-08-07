\version "2.16.0"

\header {
  texidoc = "Correct handling of PDF metadata.  LilyPond uses UTF-16BE
encoding with BOM.

In this test, the @code{title} and @code{poet} fields contain some
characters with Unicode values larger than U+00FF (Russian letters, Euro,
etc.), while the @code{composer} field uses only characters below this
threshold.  As a stress test, they also contain a closing parenthesis, which
needs to be escaped in the metadata strings by a backslash @emph{after}
encoding."

  title = "UTF-16BE title: ² € ĂĄœŖŮůſЖюљ)\\\n ¡"
  composer = "Latin1 composer: Jöhånñ Strauß"
  poet = "UTF-16BE poet: ) € ĂĄœŖŮůſЖюљ"
}

\score
{
  \new Staff c'1
}
