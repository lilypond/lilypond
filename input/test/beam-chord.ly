\version "1.5.68"

\header {
texidoc = "Hmm. what's this supposed to test?"
}
\score{
	\notes\transpose c'{
\property Voice.Beam \set #'position-callbacks =
 #`(,Beam::least_squares
			       ,Beam::check_concave
			       ,Beam::slope_damping
			       ,Beam::shift_region_to_valid
	
			      )
		[a'8 <a' g''>]
		[c <c e,>]
		[a'16 <a' g''>]
		[c <c e,>]
		[a'32 <a' g''>]
		[c <c e,>]
	}
	\paper{

		linewidth = 66.0\mm
	}
}
