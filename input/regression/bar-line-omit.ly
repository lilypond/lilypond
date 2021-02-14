\version "2.23.1"

\header {
  texidoc = "An omitted bar line behaves like an empty bar line.  The
horizontal space between notes should be the same in both measures."
}

\layout {
  ragged-right = ##t
}

\new Score \fixed c' {
  \repeat unfold 3 { c4 \bar "" } c
  \repeat unfold 3 { c4 \once \omit Staff.BarLine \bar ":|.S.|:" } c
}
