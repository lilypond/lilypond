
\version "2.7.32"
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
	\layout{ ragged-right = ##t}
}

