
\version "2.1.7"
\header {

texidoc = "With the new chord syntax it's possible to associate
fingerings uniquely with notes. This makes horizontal fingering much
easier to process."

}
    \paper { raggedright= ##t }


\score {
    \notes \relative c'{

	%% input order is not 1 2 3 , output is.
	<c-1 g'-3 e-2  b'-4 d-5 f-6 a-7 c-8 > 4	

	\property Voice.fingeringOrientations = #'(left)
	< c-1  e-2 g-3 b-5 > 4

	\property Voice.fingeringOrientations = #'(down left)
	< c-1  e-2 g-3 b-5 > 4

	\property Voice.fingeringOrientations = #'(down left up)
	< c-1  e-2 g-3 b-5 > 4

	\once \property Voice.Fingering \set #'staff-padding = #'()
	< c-1  e-2 g-3 b-5 > 4

	\property Voice.fingeringOrientations = #'(up left)
	< c-1  e-2 g-3 b-5 > 4

	\property Voice.fingeringOrientations = #'(right)
	< c-1  e-2 g-3 b-5 > 4


	
}

}




