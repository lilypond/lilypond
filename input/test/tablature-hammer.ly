
\version "2.3.22"
\header {texidoc = "@cindex Tablature hammer
A hammer in tablature can be faked with slurs. "
} 

\score{
  \context TabStaff <<
	\relative c''{
		c(d)
		d(d)
		d(c)
  }
  >>
	\layout{ raggedright = ##t}
}

