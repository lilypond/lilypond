\version "2.23.12"

\header {
  texidoc = "Test predefined bar types at the beginning, middle, and
end of a line.  The types in this group are intended for use as
measure bar lines."
}

\paper { ragged-right = ##t }

testBar = "|"
\include "bar-line-built-in-test.ily"

testBar = "-span|" % mensurstrich
\include "bar-line-built-in-test.ily"
