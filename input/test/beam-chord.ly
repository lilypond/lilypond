\version "1.7.18"
\header {
texidoc = "MOVE TO REGTEST.
If the first and last outer notes of a beam are the same, the beam should be
be horizontal.
"
}
\score{
	\notes	\transpose c' c'{

% this property doesn't appear to change the output in 1.7.19.
\property Voice.Beam \set #'position-callbacks =
 #`(,Beam::least_squares
			       ,Beam::check_concave
			       ,Beam::slope_damping
			       ,Beam::shift_region_to_valid
	
			      )
		 a'8-[ <<a' g''>>]
		 c-[ <<c e,>>]
		 a'16-[ <<a' g''>>]
		 c-[ <<c e,>>]
		 a'32-[ <<a' g''>>]
		 c-[ <<c e,>>]
	}
	\paper{

		linewidth = 66.0\mm
	}
}
%% new-chords-done %%
