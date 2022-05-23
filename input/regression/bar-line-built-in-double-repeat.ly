\version "2.23.1"

\header {
  texidoc = "Test predefined bar types at the beginning, middle, and
end of a line.  The types in this group are intended for use where one
repeated section ends and another begins."
}

\paper { ragged-right = ##t }

testBar = ":..:"
\include "bar-line-built-in-test.ily"

testBar = ":|.:"
\include "bar-line-built-in-test.ily"

testBar = ":|.|:"
\include "bar-line-built-in-test.ily"

testBar = ":.|.:"
\include "bar-line-built-in-test.ily"

testBar = ":|][|:"
\include "bar-line-built-in-test.ily"
