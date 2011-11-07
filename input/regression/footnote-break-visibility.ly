\version "2.15.17"
\header {
  texidoc = "With grobs that have break visibility, footnotes will
automatically take the break visibility of the grob being footnoted.
This behavior can be overridden.
"
}

#(set-default-paper-size "a6")

\book {

\new Staff \with { \consists "Footnote_engraver" }
{
  \relative c' {
    c1
    \footnoteGrob #'TimeSignature #'(0 . 2) "foo" "bar"
    \time 3/4
    \break \pageBreak
    c2.
    \once \override Staff . FootnoteItem #'break-visibility = ##(#f #f #t)
    \footnoteGrob #'TimeSignature #'(0 . 2) "foo" "bar"
    \time 4/4
    \break \pageBreak
    c1 \bar "|."
}}}
