\header {
  texidoc = "Text and trill spanners are attached to note columns, so
  attachments in other staves have no effect on them."
}

\layout {
  ragged-right = ##t
}

\version "2.17.6"
<<
  \new Staff {
    \override TextSpanner.bound-details.left.text = "*"
    c'16 \startTrillSpan c' c' c' \stopTrillSpan
    c'16 \startTextSpan c' c' c' \stopTextSpan
  }
  \new Staff {
    \override TextScript.self-alignment-X = #RIGHT
    \repeat unfold 2 {c'4 _ \markup { "LONG" } }
  }
>>
