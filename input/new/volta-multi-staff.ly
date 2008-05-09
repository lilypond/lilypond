\version "2.11.36"
\layout { ragged-right= ##t }
\header {
  lsrtags = "repeats,staff-notation"
  texidoc = "By adding the @code{Volta_engraver} to the relevant
staff, volte can be put over staves other than the topmost
one in a score."
  doctitle = "Volta multi-staff"
}

% LSR: this is a test which should get automatically
% LSR: removed from input/lsr/    -gp
voltaMusic = \relative c'' {
  \repeat volta 2
    c1 
    \alternative { 
    d e
   } 
} 

<<
  \new StaffGroup <<
    \context Staff \voltaMusic
    \new Staff \voltaMusic
  >>
  \new StaffGroup <<
    \new Staff \with { \consists "Volta_engraver" }
      \voltaMusic
    \new Staff \voltaMusic
  >>
>>
