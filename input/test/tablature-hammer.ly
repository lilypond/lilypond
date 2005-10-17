
\version "2.7.13"
\header {texidoc = "@cindex Tablature hammer
A hammer in tablature can be faked with slurs. "
} 

\score{
  %BROKEN \context TabStaff
  <<
	\relative c''{
		c(d)
		d(d)
		d(c)
  }
  >>
	\layout{ raggedright = ##t}
}

