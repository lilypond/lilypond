\version "2.27.5"

\header { texidoc = "Properly combine parts with consecutive dynamic spanners." }

\layout { ragged-right = ##t }

common = { c''1\p c''1\< c''1\<   c''1\f }
part_one = { R1 c'1\p\< c'1\<   c'1\f }

part_two = { R1 c'1\p\< c'1\!\> c'1\f }

\new StaffGroup
<<
  \new Staff \with { instrumentName = "I" } { \common \common }
  \new Staff \with { instrumentName = "II" } { \part_one \part_two }
>>

\new Staff \with { instrumentName = "I+II" }
  { \partCombine \common \part_one \partCombine \common \part_two }
