\version "2.23.0"

\header {
  texidoc = "Grace-timed elements in sequence line up before the next
main note in the obvious way."
}

x = \override NoteHead.style = #'cross
o = \revert NoteHead.style

\fixed c' <<
  %% grace notes starting at the beginning of the score
  \new Staff { \grace c8 \grace d8 \grace e8           }
  \new Staff {           \grace d8 \appoggiatura e8 f1 }
  %% grace notes starting in the middle of the score
  \new Staff { f2              \grace d8    \grace e8 f2 }
  \new Staff { f2 \grace c8 \x \grace d8 \o \grace e8 }
>>
