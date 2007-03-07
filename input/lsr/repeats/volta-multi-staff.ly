\version "2.10.12"

\header { texidoc = "
By setting @{voltaOnThisStaff@}, repeats can be put also over other
staves than the topmost one in a score.
" }

\layout {
  ragged-right = ##t 
}

vmus =  {
  \repeat volta 2 c1 \alternative { d e } 
} 

\relative c'' <<
  \new StaffGroup <<
    \context Staff \vmus
    \new Staff \vmus
  >>
  \new StaffGroup <<
    \new Staff <<
      \set Staff.voltaOnThisStaff = ##t
      \vmus >>
    \new Staff \vmus
  >>
>>
