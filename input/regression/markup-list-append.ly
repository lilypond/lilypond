\version "2.21.5"

\header {
  texidoc = "This concatenates the same markup list several times."
}

sample = \markuplist { Test }

\markup {
  { \sample \sample \sample . }
}
