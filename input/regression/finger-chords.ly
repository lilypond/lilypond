
\version "2.3.17"
\header {

texidoc = "With the new chord syntax, it is possible to associate
fingerings uniquely with notes. This makes horizontal fingering much
easier to process."

}
    \paper { raggedright= ##t }


\score {
     \relative c'{

	%% input order is not 1 2 3 , output is.
	<c-1 g'-3 e-2  b'-4 d-5 f-6 a-7 c-8 > 4	

	\set fingeringOrientations = #'(left)
	< c-1  e-2 g-3 b-5 > 4

	\set fingeringOrientations = #'(down left)
	< c-1  e-2 g-3 b-5 > 4

	\set fingeringOrientations = #'(down left up)
	< c-1  e-2 g-3 b-5 > 4

	\once \override Fingering  #'staff-padding = #'()
	< c-1  e-2 g-3 b-5 > 4

	\set fingeringOrientations = #'(up left)
	< c-1  e-2 g-3 b-5 > 4

	\set fingeringOrientations = #'(right)
	< c-1  e-2 g-3 b-5 > 4


	
}

}




