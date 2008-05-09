\version "2.11.46"

\header {

  texidoc = "'to-barline is not confused by very long marks."

}

\new Staff \relative c' {
  c1\<
  \mark "Very long mark"
  c4\> c c c\!
}
