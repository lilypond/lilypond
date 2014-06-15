\version "2.19.24"

\header
{ texidoc = "Partial markups acts as a chain of markup
  commands where everything but the final markup has already been
  supplied."
}

\layout { ragged-right = ##t }

bold-red-markup = \markup \bold \with-color #red \etc

\markup \bold-red "Single markup"
\markuplist \column-lines \bold-red { Markups in a list. }
