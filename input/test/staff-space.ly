\version "2.1.26"

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
      \override StaffSymbol #'staff-space = #5.5
      }
	raggedright=##t }
}

