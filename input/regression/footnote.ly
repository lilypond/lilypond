\version "2.15.25"
\header {
  texidoc = "Lilypond does footnotes."
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
\footnote \markup { \tiny 4 } #'(1 . -1) #'NoteHead \markup { 4. j }
a b c d }

\pageBreak

\markup { k \footnote \concat { l \super 5 } \line { 5. m }  }

\relative c' { a1 }

\relative c' {
  d4 e
  < f  a-\footnote \markup { \tiny 6 } #'(1 . -1) \markup { 6. n } c >
  \footnote \markup { \tiny 7 } #'(1 . 1) #'Beam \markup { 7. o }
  \footnote \markup { \tiny 8 } #'(1 . 1) #'Hairpin \markup { 8. p }
  a8\< [ b c d\f ] r2. |
}}
