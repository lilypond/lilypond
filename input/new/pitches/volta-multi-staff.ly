\header {
  texidoc = "By adding @code{Volta_engraver}, repeat brackets
  can be put over staves other than the topmost one in a score."
}

\version "2.11.36"

vmus =  {
  \repeat volta 2 c1 \alternative { d e } 
} 

\relative c'' <<
  \new StaffGroup <<
    \context Staff \vmus
    \new Staff \vmus
  >>
  \new StaffGroup <<
    \new Staff \with { \consists Volta_engraver }
      \vmus
    \new Staff \vmus
  >>
>>
