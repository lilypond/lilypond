\score{
	\context Staff \notes\relative c'{ 
		% compare this to 'semi' with numbered voltas:
		%  1-3: alternative 1
		%    4: alternative 2
		\repeat unfold 4 { g a b c } 
		\alternative { 
			{ c c c c }
			{ d d d d }
		}
	}
}
