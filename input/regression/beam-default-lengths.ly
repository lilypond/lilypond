
\version "2.1.7"

\header{
    texidoc="Beamed stems have standard lengths if possible. Quantization is switched off in this example."
    }

\score{
    \notes\relative c'{
	\property Voice.Beam \set #'position-callbacks =
	 #`(,Beam::least_squares
	 ,Beam::check_concave
	 ,Beam::slope_damping)

        f4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f] 
    }
    \paper{ raggedright = ##t}
}          
