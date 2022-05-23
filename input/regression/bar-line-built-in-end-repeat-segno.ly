\version "2.23.1"

\header {
  texidoc = "Test predefined bar types at the beginning, middle, and
end of a line.  The types in this group are intended for use where a
repeated section ends and there is an in-staff segno."
}

\paper { ragged-right = ##t }

testBar = ":|.S"
\include "bar-line-built-in-test.ily"

testBar = ":|.S-S"
\include "bar-line-built-in-test.ily"
