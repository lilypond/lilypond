
\version "2.1.26"
\header {texidoc = "@cindex Tablature hammer
You can fake a hammer in tablature with slurs. "
} 

\score{
  \context TabStaff <<
	\notes\relative c''{
		c(d)
		d(d)
		d(c)
  }
  >>
	\paper{ raggedright = ##t}
}

