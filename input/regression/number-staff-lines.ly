
\version "2.1.23"

\header{
texidoc=" The number of stafflines of a staff can be set.  Ledger
lines both on note heads and rests are adjusted.  Barlines also are
adjusted.  "
}

\score { 
\context Voice \notes\relative c {
	c' c c c | g' g g g 	\bar ":|"
  }
  \paper {

\translator { \StaffContext
\override StaffSymbol #'line-count = #3
}  }
  \midi { }
}

