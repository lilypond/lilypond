\header {
  texidoc = "By putting Volta_engraver in a staff context, one can get
volta brackets on staves other than the topmost one."
}

\layout {
  ragged-right = ##t

  \context {
    \Score
    \remove "Volta_engraver"
  }
}

\version "2.16.0"

vmus =  { \repeat volta 2 c1 \alternative { d e } } 

\relative c'' <<
  \new StaffGroup <<
    \new Staff \vmus
    \new Staff \vmus
  >>
  \new StaffGroup <<
    \new Staff \with { \consists "Volta_engraver" }
      \vmus
    \new Staff \vmus
  >>
>>
