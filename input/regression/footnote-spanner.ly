\version "2.23.4"
\header {
  texidoc = "Footnotes are annotated at the correct place, and the
annotation goes to the correct page."
}

#(set-default-paper-size "a6")

\paper {
  ragged-last-bottom = ##f
}

\book {

\relative {
b'4-\single\footnote
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

\once \override Score.Footnote.spanner-placement = #RIGHT
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
