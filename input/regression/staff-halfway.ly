
\header { texidoc = " Staves starting and ending halfway include clefs
    and bar lines.  " }

\version "2.3.4"

\score {
    \new StaffGroup  \relative c''  <<
	\new Staff { c4  c c c \bar "||" c c c c }
	{ \skip 4 \new Staff { c c c } }
    >>
	\paper {}	       
	 }
