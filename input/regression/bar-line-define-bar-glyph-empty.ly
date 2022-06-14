\version "2.23.10"

\header {
  texidoc = "A user-defined empty bar glyph behaves like the built-in
empty bar glyph.  The horizontal space between notes should be the same
in both measures."
}

\layout {
  ragged-right = ##t
}

#(add-bar-glyph-print-procedure "0" (@@ (lily) make-empty-bar-line))
\defineBarLine "0" #'(#t #t #f)

\new Score \fixed c' {
  \repeat unfold 3 { c4 \bar "" } c
  \repeat unfold 3 { c4 \bar "0" } c
}
