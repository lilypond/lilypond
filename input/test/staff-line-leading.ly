\version "1.5.68"
\header{

    texidoc ="Setting staff space on a staff "
    }
\score { 
  \notes \relative c'' \context GrandStaff <
	\context Staff = up { c4 c4  }
	\context Staff = down {  c4
	  [<c8 d f g>
	   e]
	  [f c']
	 }
>
\paper {  \translator  {
      \StaffContext
      StaffSymbol \set #'staff-space = #5.5
      }}
}
