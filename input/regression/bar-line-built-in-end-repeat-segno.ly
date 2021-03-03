\version "2.23.1"

\header {
  texidoc = "Test bar lines at beginning, middle, and end of line."
}

\paper { ragged-right = ##t }

testBar = ":|.S"
\include "bar-line-built-in-test.ily"

testBar = ":|.S-S"
\include "bar-line-built-in-test.ily"
