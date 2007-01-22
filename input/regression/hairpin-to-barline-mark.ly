\version "2.11.12"
\paper { ragged-right = ##t }
\header {

  texidoc = "hairpinToBarline is not confused by very long marks."

}

\new Staff \relative c' {
  c1\< |
  \mark "Very long mark"
  c4\> c c c\! |
}
