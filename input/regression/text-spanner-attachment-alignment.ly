\header {
  texidoc = "Text and trill spanners are attached to note columns, so
  attachments in other staves have no effect on them."
}

\layout {
  ragged-right = ##t
}

\version "2.25.35"

<<
  \new Staff {
    \override TextSpanner.bound-details.left.text = "*"
    c'16 \startTrillSpan c' c' c' \stopTrillSpan
    c'16 \startTextSpan c' c' c' \stopTextSpan
  }
  \new Staff {
    \override TextScript.self-alignment-X = #RIGHT
    \*2 { c'4_\markup { "LONG" } }
  }
>>
