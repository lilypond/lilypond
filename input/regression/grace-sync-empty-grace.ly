\version "2.27.5"

\header {
  texidoc = "
In the non-standard situation of staves with key changes after grace
notes, moving the key change in a staff having none can be avoided by
including an empty grace group."
}

\layout { ragged-right = ##t }

<<
  \new Staff { c''2 \grace { c''16 d'' e'' f'' } \key f \major e''2 }
  \new Staff { c''2 \grace { g'8 b' } \key f \major c''2 }
  \new Staff { c'2  \grace { } \key f \major c'2 }
>>
