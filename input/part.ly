\version "1.0.7";
part = \notes {
	c-1 c c c
	r1*3
	c4-5 c c c
	c-6 c c c
	c-7 c c c
	c-8 c c c
}

a4 = \paper{
	linewidth= 80.\mm;
	 \translator {
	 	\StaffContext
		
		\consists "Bar_column_engraver";
		\consists "Bar_number_engraver";
		
	}
}

\score{
	<
		\notes{ 
			\part
		}
	>
	\paper{\a4}
}

\score{
	<
		\notes{ 
			\part
		}
	>
	\paper{\a4}
}
