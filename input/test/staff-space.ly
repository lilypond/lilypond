\version "2.3.8"

\header{ texidoc ="@cindex Staff Space
By just increasing @code{staff-space} on a staff, you can produce strange
results. "
}

\score { 
   \relative c'' \context GrandStaff <<
	\new Staff { c4 c4  }
	\new Staff {
	    c4

	    <c d f g>8[
	   e]
	   f[ c']
	 }
>>
\paper {  \context  {
      \Staff
      \override StaffSymbol #'staff-space = #5.5
      }
	raggedright=##t }
}

