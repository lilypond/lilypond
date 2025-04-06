\version "2.25.26"

\header {
  texidoc = "Partial markups acts as a chain of markup
commands where everything but some arguments of the final markup
command has already been supplied."
}

\layout { ragged-right = ##t }

\markup bold-red = \markup \bold \with-color #red \etc
\markup italic-color = \markup \italic \with-color \etc
\markup quarter = \markup \note-by-number #2 \etc
\markup test-column = \markup \column \etc

\markup \bold-red "Bold red."
\markuplist \column-lines \bold-red { Bold red in a list. }
\markup #(markup #:italic-color green "Italic green.")
\markuplist \column-lines \italic-color #green { Italic green in a list. }
\markup { 3/8: #(make-quarter-markup 1 UP) }
\markup \test-column { "Row 1 of 2" "Row 2 of 2" }
