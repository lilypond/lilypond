\version "2.13.54"
\header {
  texidoc = "Footnotes are annotated at the correct place, and the
annotation goes to the correct page."
}

#(set-default-paper-size "a6")

\paper { ragged-last-bottom = ##f }

\book {

\relative c'' {
\once \override FootnoteSpanner #'spanner-placement = #-0.7
\footnoteGrob #'Hairpin
              #'(0.5 . 0.5)
              \markup { \tiny "1." }
              \markup { 1. \justify { Goes to the second broken spanner. } }
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

\once \override FootnoteSpanner #'spanner-placement = #1.0
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
