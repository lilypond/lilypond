\version "1.5.68"



\score {
\notes <
	\context GrandStaff < {
	   c'1 \break  c'''''1 
	}
	    { c'1 \break c,,,,1}
	>

>

\paper{
  \translator{\StaffContext
    VerticalAlignment \override #'threshold = #'(7 . 12)
  }
}

}
