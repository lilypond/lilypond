\version "1.5.68"

\header{
    texidoc="Beams in unnatural direction, have shortened stems, but do not look too short."
    }

\score{
    \notes\relative c'{
	\property Voice.Beam \set #'position-callbacks =
	 #`(,Beam::least_squares
	 ,Beam::check_concave
	 ,Beam::slope_damping)
	\stemUp
        f'4 [f8 f] [f16 f] [f32 f] [f64 f] [f128 f] 
    }
    \paper{ linewidth = -1.0 }
}          
