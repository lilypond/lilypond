\version "2.17.6"
\header {
  texidoc = "Footnotes are annotated at the correct place, and the
annotation goes to the correct page."
}

#(set-default-paper-size "a6")

\paper {
  ragged-last-bottom = ##f
}

\book {

\relative c'' {
b4-\single\footnote
              \markup { \tiny "1." }
              #'(0.5 . 0.5)
              \markup { 1. \justify { Goes to the first broken spanner. } } Hairpin
   \<
c d a
b c d a
b c d a
b c d a
b c d a
b c d a \break \pageBreak
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a\!

\once \override FootnoteSpanner #'spanner-placement = #RIGHT
b4-\single\footnote
              \markup { \tiny "2." }
              #'(0.5 . 0.5)
              \markup { 2. \justify { Goes to the last broken spanner. } } Hairpin
   \<
c d a
b c d a
b c d a
b c d a
b c d a
b c d a \break \pageBreak
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d a
b c d\!
}}
