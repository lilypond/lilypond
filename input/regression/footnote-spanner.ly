\version "2.14.0"
\header {
  texidoc = "Footnotes are annotated at the correct place, and the
annotation goes to the correct page."
}

#(set-default-paper-size "a6")

\paper {
  ragged-last-bottom = ##f
  footnote-auto-numbering = ##f
}

\book {

\relative c'' {
\footnoteGrob #'Hairpin
              #'(0.5 . 0.5)
              \markup { \tiny "1." }
              \markup { 1. \justify { Goes to the first broken spanner. } }
b4\< c d a
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
\footnoteGrob #'Hairpin
              #'(0.5 . 0.5)
              \markup { \tiny "2." }
              \markup { 2. \justify { Goes to the last broken spanner. } }
b4\< c d a
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
