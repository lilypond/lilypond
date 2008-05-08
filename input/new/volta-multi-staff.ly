\version "2.11.36"
\layout { ragged-right= ##t }
\header {
  lsrtags = "repeats,staff-notation"
  texidoc = "By adding @code{Volta_engraver}, repeat brackets
can be put over staves other than the topmost one in a score."
  doctitle = "Volta multi-staff"
}

% LSR: this is a test which should get automatically
% LSR: removed from input/lsr/    -gp
vmus = \relative c'' {
  \repeat volta 2 c1 \alternative { d e } 
} 

<<
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
