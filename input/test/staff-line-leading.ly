\version "1.7.16"
\header{

    texidoc ="Setting staff space on a staff "
    }
\score { 
  \notes \relative c'' \context GrandStaff <
	\context Staff = up { c4 c4  }
	\context Staff = down {  c4
	  [<<c d f g>>8
	   e]
	  [f c']
	 }
>
\paper {  \translator  {
      \StaffContext
      StaffSymbol \set #'staff-space = #5.5
      }}
}
%% new-chords-done %%
