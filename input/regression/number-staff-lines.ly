
\version "2.3.16"

\header{
texidoc=" The number of stafflines of a staff can be set.  Ledger
lines both on note heads and rests, as well as barlines, are
adjusted accordingly.  "
}

\score { 
\context Voice \relative c {
	c' c c c | g' g g g 	\bar ":|"
  }
  \paper {

\context { \Staff
\override StaffSymbol #'line-count = #3
}  }
  \midi { }
}

