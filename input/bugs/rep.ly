\score{
	<
		  \context Staff \notes\relative c'{ 
			  c4 d e f
			  \repeat semi 3 { g a b c }
			  \alternative { { c b a g } { f e d c } } c c c c
%			  \alternative { { c b a g } { f e d c } { c d e f } }
			  g g g g
			  \repeat semi 2 { c c c c }
			  \repeat semi 2 { c c c c }
			  g g g g
		  }
	>
}
