\version "2.19.21"
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
\footnote \markup { \tiny 4 } #'(1 . -1) \markup { 4. j }
a b c d }

\pageBreak

\markup { k \footnote \concat { l \super 5 } \line { 5. m }  }

\relative { a1 }

\relative {
  d'4 e
  < f \footnote \markup { \tiny 6 } #'(1 . -1) \markup { 6. n } a c >
  a8-\footnote \markup { \tiny 8 } #'(1 . 1) \markup { 8. p } \<
  -\footnote \markup { \tiny 7 } #'(1 . 1) \markup { 7. o }
  [ b c d\f ] r2. |
}}
