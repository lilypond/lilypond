\version "2.23.6"
#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "The @code{Mark_@/tracking_@/translator} manages one
rehearsal-mark sequence for (potentially) many
@code{Mark_@/engravers}.  The expected marks on both staves are these:
1, 2, 9, 10, 12, 13, 20,@tie{}21."
}

\layout {
  \context {
    \Score
    \remove "Mark_engraver"
    rehearsalMarkFormatter = #format-mark-numbers
  }
  \context {
    \Staff
    \consists "Mark_engraver"
  }
}

md = \mark \default
twenty = \set Score.rehearsalMark = 20

<<
  \new Staff { \md R1     R \md     R     R \mark 12 R     R \md     R \md }
  \new Staff {     R1 \md R \mark 9 R \md R \md      R \md R \twenty R \md }
>>
