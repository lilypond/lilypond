\version "2.19.21"
\header {
  texidoc = "This is an example of automatic footnote numbering
where the number is reset on each page.  It uses the symbol-footnotes
numbering function, which assigns the symbols *, †, ‡, § and ¶ to
successive footnotes, doubling up on the symbol after five footnotes
have been reached.
"
}

\paper {
  footnote-numbering-function = #symbol-footnotes
}

#(set-default-paper-size "a6")
\book {

\markup {
  a \auto-footnote b c
  \auto-footnote d e
  \auto-footnote f g
}

\markup { h i }

\relative c' {
\footnote #'(1 . -1) \markup { j } NoteHead
a b c d }

\pageBreak

\markup { k \auto-footnote l m }

\relative { a1 }

\relative {
  d'4 e
  < f  \footnote #'(1 . -1) \markup { n } a c >
  a8-\footnote #'(1 . 1) \markup { p } \<
  -\footnote #'(1 . 1) \markup { o } [ b c d ] a4 b c |
  d a b c |
  d a b c |
  d a b c\f |
}}
