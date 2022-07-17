\version "2.23.12"

\header {
  texidoc = "Text replacements can replace strings with arbitrary markups."
}

\markup \replace #`(("2nd" . ,#{ \markup \concat { 2 \super nd } #})) "2nd time"
