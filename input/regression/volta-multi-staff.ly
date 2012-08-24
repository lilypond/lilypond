\header {
  texidoc = "By default, the volta brackets appear only in the topmost staff."
}

\layout { ragged-right = ##t }
\version "2.16.0"

vmus =  { \repeat volta 2 c1 \alternative { d e } } 

\relative c'' <<
  \new StaffGroup <<
    \new Staff \vmus
    \new Staff \vmus
  >>
  \new StaffGroup <<
    \new Staff \vmus
    \new Staff \vmus
  >>
>>



