\header {

  texidoc = "hairpinToBarline is not confused by very long marks."

}

\version "2.10.12"

\paper { ragged-right = ##t }

\new Staff \relative c' {
  c1\< |
  \mark "Very long mark"
  c4\> c c c\! |
}
