\version "2.14.0"
\header {
  texidoc = "With grobs that have break visibility, footnotes will
automatically print to the first line of the break.  This behavior
can be overrided."
}

#(set-default-paper-size "a6")

\paper {
  footnote-auto-numbering = ##f
}

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
