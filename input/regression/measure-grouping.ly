\header
{
texidoc = "The Measure_grouping_engraver adds triangles and brackets above beats
when you set beatGrouping.

(unfinished.)
"
}
\score { \notes
\context Staff
	 {
	     \time 8/4
	     \property Staff.beatGrouping = #'(3 3) 
	     c4 c c c
	     c4
	     \property Staff.beatGrouping = #'()
	     c c c
  	     
	 }
	 \paper  {
	     \translator { \StaffContext
			   \consists "Measure_grouping_engraver"
			   }
	     }
	 }
