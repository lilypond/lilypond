\version "2.27.5"

\header { texidoc ="
When grace notes occur in parallel music, volta repeat brackets are
appropriately extended in front.
" }

\layout {
  ragged-right = ##t
}

<<
  \repeat volta 4 { c'1 \alternative {
    \volta 1 1
    \volta 2 { \grace 16 1 }
    \volta 3 { 1 \grace 8 } \volta 4 1 } }
  \repeat unfold 5 { \volta 1,2,4 \grace 8 1 }
>>
