\version "1.7.16"

\header{
    texidoc="Beamed stems have standard lengths if possible."
    }

\score{
    \notes\relative c'{
	\property Voice.Beam \set #'position-callbacks =
	 #`(,Beam::least_squares
	 ,Beam::check_concave
	 ,Beam::slope_damping)

        f4 [f8 f] [f16 f] [f32 f] [f64 f] [f128 f] 
    }
    \paper{ raggedright = ##t}
}          %% new-chords-done %%