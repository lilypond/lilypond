\version "2.1.26" 

\header{ texidoc="@cindex Nested Staff Groups
LilyPond can print nested groups of staffs. "

%   old texidoc; it sounds like a testing comment, rather than an "example" comment.
%In InnerStaffGroup and InnerChoirStaff, the brackets should be shiftet leftwards.
}

\score { \notes
<<
  \new StaffGroup << 
  \new Staff {c' d' e' f'}
  \new InnerStaffGroup <<
   \new Staff {c' d' e' f'}
   \new GrandStaff <<
     \new Staff {c' d' e' f'}
     \new Staff {c' d' e' f'}
   >>
  \new Staff {c' d' e' f'}
  >>
  \new ChoirStaff <<
   \new Staff {c' d' e' f'}
    \new InnerStaffGroup <<
     \new Staff {c' d' e' f'}
    >>
   \new Staff {c' d' e' f'}
  >>
  >>
  \new ChoirStaff << 
   \new Staff {c' d' e' f'}
   \new InnerStaffGroup <<
    \new Staff {c' d' e' f'}
    \new Staff {c' d' e' f'}
   >>
   \new Staff {c' d' e' f'}
  >>

>>

 \paper { raggedright = ##t}
}

