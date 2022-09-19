\version "2.23.14"

\header {

  texidoc = "'to-barline is not confused by very long marks."

}

\new Staff \relative {
  c'1\<
  \tweak self-alignment-X #CENTER
    \textMark "This is quite a long mark text"
  c4\> c c c\!
}
