\version "1.5.68"


\score {
\notes { c1 c1 \break c1 c1 }
\paper {

\translator{
	\StaffContext
	\consists Bar_number_engraver
	barNumberVisibilityFunction = #end-of-line-invisible
}
}
}

