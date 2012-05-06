\version "2.15.39"
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
<>\footnote #'(1 . -1) #'NoteHead \markup { j }
a b c d }

\pageBreak

\markup { k \auto-footnote l m }

\relative c' { a1 }

\relative c' {
  d4 e
  < f  a\footnote #'(1 . -1) \markup { n } c >
  <>\footnote #'(1 . 1) #'Beam \markup { o }
  <>\footnote #'(1 . 1) #'Hairpin \markup { p }
  a8\< [ b c d ] a4 b c |
  d a b c |
  d a b c |
  d a b c\f |
}}
