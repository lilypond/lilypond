\version "2.23.10"

\header {
  texidoc = "Test predefined bar types at the beginning, middle, and
end of a line.  The types in this group are intended for use between
sections."
}

\paper { ragged-right = ##t }

testBar = "||"
\include "bar-line-built-in-test.ily"

testBar = "."
\include "bar-line-built-in-test.ily"

testBar = "|.|"
\include "bar-line-built-in-test.ily"
