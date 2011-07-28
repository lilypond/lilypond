\version "2.14.0"
\header {
  texidoc = "Lilypond does footnotes."
}

\paper {
  footnote-auto-numbering = ##f
}

#(set-default-paper-size "a6")
\book {

\markup {
  a \footnote \concat { b \super 1 } "1. c"
  \footnote \concat { d \super 2 } "2. e"
  \footnote \line { f \super 3 } "3. g"
}

\markup { h i }

\relative c' {
\footnoteGrob #'NoteHead #'(1 . -1) \markup { \tiny 4 } \markup { 4. j }
a b c d }

\pageBreak

\markup { k \footnote \concat { l \super 5 } \line { 5. m }  }

\relative c' { a1 }

\relative c' {
  d4 e
  < f  a-\footnote #'(1 . -1) \markup { \tiny 6 } \markup { 6. n } c >
  \footnoteGrob #'Beam #'(1 . 1) \markup { \tiny 7 } \markup { 7. o }
  \footnoteGrob #'Hairpin #'(1 . 1) \markup { \tiny 8 } \markup { 8. p }
  a8\< [ b c d\f ] r2. |
}}
