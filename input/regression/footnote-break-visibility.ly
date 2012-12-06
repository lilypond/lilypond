\version "2.17.6"
\header {
  texidoc = "With grobs that have break visibility, footnotes will
automatically take the break visibility of the grob being footnoted.
This behavior can be overridden.
"
}

#(set-default-paper-size "a6")

\book {

\new Staff
{
  \relative c' {
    c1
    \footnote "foo" #'(0 . 2) "bar" Staff.TimeSignature
    \time 3/4
    \break \pageBreak
    c2.
    \once \override Score.FootnoteItem.break-visibility = ##(#f #f #t)
    \footnote "foo" #'(0 . 2) "bar" Staff.TimeSignature
    \time 4/4
    \break \pageBreak
    c1 \bar "|."
}}}
