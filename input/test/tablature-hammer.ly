\version "1.5.68"
\header {
texidoc = "" 
}

\score{
  \context TabStaff <
	\notes\relative c''{
		c()d
		d()d
		d()c
  }
  >
}
