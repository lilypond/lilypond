\version "2.23.6"

\header {
  texidoc= "Marks can be printed as numbers.  By setting
@code{rehearsalMarkFormatter} we may choose a different style of mark
printing.  Also, marks can be specified manually, with a markup
argument."
}

\paper { ragged-right = ##t }

\relative c''{
  \set Score.rehearsalMarkFormatter = #format-mark-numbers
  c1 | \mark \markup { \musicglyph "scripts.coda" }
  c1 | \mark \default
  c1 | \mark \default
  \set Score.rehearsalMarkFormatter
  = #(lambda (mark  context)
      (make-box-markup (format-mark-numbers mark context)))
  c1 | \mark \default
  \set Score.rehearsalMarkFormatter
  = #(lambda (mark  context)
      (make-circle-markup (format-mark-numbers mark context)))
  c1 | \mark \default
}
