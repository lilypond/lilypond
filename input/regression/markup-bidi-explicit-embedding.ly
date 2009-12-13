\version "2.13.10"

\header {
  texidoc = "
The explicit directional embedding codes, U+202A and U+202B, are
supported in single-line markup strings.  The embeddings must be
terminated with the pop directional formatting character, U+202C.
"
}

\markup {
  \column {
    "אבה אבה \"ABC!\" אבה אבה!"
    "אבה אבה \"‪ABC!‬\" אבה אבה!"
    \null
    "abc def \"אבה!\" ghi jkl!"
    "abc def \"‫אבה!‬\" ghi jkl!"
  }
}
