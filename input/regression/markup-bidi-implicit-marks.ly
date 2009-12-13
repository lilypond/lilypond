\version "2.13.10"

\header {
  texidoc = "
The implicit directional marks, U+200E and U+200F, are supported
in single-line markup strings.
"
}

\markup {
  \column {
    "אבה \"ABC!\" אבה"
    "אבה \"ABC!‎\" אבה"
    \null
    "abc \"אבה!\" def"
    "abc \"אבה!‏\" def"
  }
}
