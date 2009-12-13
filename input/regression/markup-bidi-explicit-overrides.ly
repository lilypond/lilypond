\version "2.13.10"

\header {
  texidoc = "
The explicit directional override codes, U+202D and U+202E, are
supported in single-line markup strings.  The overrides must be
terminated with the pop directional formatting character, U+202C.
"
}

\markup {
  \column {
    "אבג דהו זחט יךכ"
    "‭אבג דהו זחט יךכ‬"
    \null
    "abc def ghi jkl"
    "‮abc def ghi jkl‬"
  }
}
