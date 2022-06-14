\version "2.23.12"

\header {
  texidoc = "Test predefined bar types at the beginning, middle, and
end of a line.  The types in this group are intended for a caesura at
a line break."
}

\paper { ragged-right = ##t }

testBar = "x-|"
\include "bar-line-built-in-test.ily"

testBar = "x-||"
\include "bar-line-built-in-test.ily"

testBar = "x-."
\include "bar-line-built-in-test.ily"
