\version "2.15.7"
\header {
  texidoc = "This is an example of automatic footnote numbering
where the number is not reset on each page.  It uses the default
numbering function, which assigns numbers starting at 1 to successive
footnotes.
"
}

\paper {
  reset-footnotes-on-new-page = ##f
}

#(set-default-paper-size "a6")
\book {

\markup {
  a \footnote b c
  \footnote d e
  \footnote f g
}

\markup { h i }

\relative c' {
\autoFootnoteGrob #'NoteHead #'(1 . -1) \markup { j }
a b c d }

\pageBreak

\markup { k \footnote l m }

\relative c' { a1 }

\relative c' {
  d4 e
  < f  a-\autoFootnote #'(1 . -1) \markup { n } c >
  \autoFootnoteGrob #'Beam #'(1 . 1) \markup { o }
  \autoFootnoteGrob #'Hairpin #'(1 . 1) \markup { p }
  a8\< [ b c d ] a4 b c |
  d a b c |
  d a b c |
  d a b c\f |
}}
