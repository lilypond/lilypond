\version "1.5.68"

\score { 
  \notes \relative c'' \context GrandStaff <
	\context Staff = up { c4 c4  }
	\context Staff = down { \property Staff. staffSpace = #5.5 c4
	  [<c8 d f g>
	   e]
	  [f c']
	 }
>
}
