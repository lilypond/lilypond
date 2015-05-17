\version "2.19.21"

\header {

  texidoc = "'to-barline is not confused by very long marks."

}

\new Staff \relative {
  c'1\<
  \mark "Very long mark"
  c4\> c c c\!
}
