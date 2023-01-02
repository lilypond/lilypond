\version "2.25.1"

\header {
  texidoc = "The file extension of a file passed to
@code{\\markup \\image} can be uppercased."
}

\markup \image #Y #30 "lilypond.PNG"
\markup \image #Y #30 "lilypond.EPS"
