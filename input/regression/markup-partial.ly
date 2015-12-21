\version "2.19.34"

\header {
  texidoc = "Partial markups acts as a chain of markup
commands where everything but some arguments of the final markup
command has already been supplied."
}

\layout { ragged-right = ##t }

bold-red-markup = \markup \bold \with-color #red \etc
italic-color-markup = \markup \italic \with-color \etc
quarter-markup = \markup \note-by-number #2 \etc

\markup \bold-red "Bold red."
\markuplist \column-lines \bold-red { Bold red in a list. }
\markup \italic-color #green "Italic green."
\markuplist \column-lines \italic-color #green { Italic green in a list. }
\markup { 3/8: \quarter #1 #UP }
