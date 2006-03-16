\version "2.7.39" 

\header{ texidoc="@cindex Nested Staff Groups
Staffs can be nested in various combinations. Here, @code{StaffGroup}
and @code{ChoirStaff} produce similar straight brackets, whereas 
@code{GrandStaff} produces curly brackets. In @code{InnerStaffGroup} 
and @code{InnerChoirStaff}, the brackets are shifted leftwards.

"
}

\score { 
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
   \new InnerChoirStaff <<
    \new Staff {c' d' e' f'}
    \new Staff {c' d' e' f'}
   >>
   \new Staff {c' d' e' f'}
  >>

>>

 \layout { ragged-right = ##t}
}

