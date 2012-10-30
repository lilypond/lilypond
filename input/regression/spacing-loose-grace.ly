
\header

{

  texidoc = "With @code{strict-grace-spacing}, grace notes don't influence
spacing."

}

\version "2.17.6"
\paper {
  ragged-right = ##t
}

<<
  \override Score.SpacingSpanner.strict-grace-spacing = ##t
  \new Staff {
     c'4
     \afterGrace
     c'4
     { c'16[ c'16 c'16 c'16] }
     c'4
  }
  \new Staff {
     c'16[ c'16 c'16 c'16]
     c'16[ c'16 c'16 c'16]
     c'4
  }
>>
