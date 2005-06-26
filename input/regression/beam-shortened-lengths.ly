
\version "2.6.0"

\header{
    texidoc="Beams in unnatural direction, have shortened stems, but do not look too short."
    }

\score{
    \relative c'{
	\override Beam  #'position-callbacks =
	 #`(,Beam::least_squares
	 ,Beam::check_concave
	 ,Beam::slope_damping)
	\stemUp
        f'4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f] 
    }
    \layout{ raggedright = ##t}
}          
