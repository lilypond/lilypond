
\header
{

    texidoc = "The Measure_grouping_engraver adds triangles and
brackets above beats when you set beatGrouping.  This shows a fragment
of Steve Martland's Dance Works. "

}

\version "1.9.8"


%% TODO: should have 2/4 + 5/8 time sig style.
\score { \notes
\context Staff
	 \relative c' {
	     #(set-time-signature 2 4) 
	     c8 a'4 a8~
	     #(set-time-signature 5 8 '(3 2)) 
	     a8 bes4 r8 bes8->
	     \time 2/4
	     c,8 g'4 g8~
	     #(set-time-signature 5 8 '(3 2)) 
	     g8 a4 g a->
	 }
	 \paper  {
	     raggedright = ##t
	     \translator { \StaffContext
			   \consists "Measure_grouping_engraver"
			   }
	     }
	 }

