
\version "2.4.0"

\header{
    texidoc="Beamed stems have standard lengths if possible. Quantization is switched off in this example."
    }

\score{
    \relative c'{
	\override Beam  #'position-callbacks =
	 #`(,Beam::least_squares
	 ,Beam::check_concave
	 ,Beam::slope_damping)

        f4  f8[ f]  f16[ f]  f32[ f]  f64[ f]  f128[ f] 
    }
    \layout{ raggedright = ##t}
}          
