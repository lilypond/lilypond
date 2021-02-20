\version "2.23.1"

\header {
  texidoc = "text replacement settings are scoped to the @code{\\paper} block"
}

ignoreMe = \paper { 
  #(add-text-replacements! '(("good" . "BAD")))
}

\markup "good"

