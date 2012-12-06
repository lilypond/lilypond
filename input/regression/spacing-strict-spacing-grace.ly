\header {

  texidoc = "With @code{strict-note-spacing} spacing for grace notes
(even multiple ones), is floating as well."

}

\version "2.17.6"

<<
 \override Score.SpacingSpanner.strict-grace-spacing = ##t
 \new Staff {
    c'4
    \afterGrace
    c'4
    { c'16[ c' c' c'] c'[ c' c' c'] }
    c'4
 }
 \new Staff {
    c'16[ c'16 c'16 c'16]
    c'16[ c'16 c'16 c'16]
    c'4
 }
>>
