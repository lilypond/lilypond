#(ly:set-option 'old-relative)
\version "1.9.8"

\header{ texidoc ="@cindex Staff Space
Setting staff space on a staff. "
}

\score { 
  \notes \relative c'' \context GrandStaff <<
	\new Staff { c4 c4  }
	\new Staff {
	    c4

	    <c d f g>8[
	   e]
	   f[ c']
	 }
>>
\paper {  \translator  {
      \StaffContext
      StaffSymbol \set #'staff-space = #5.5
      }
	raggedright=##t }
}

