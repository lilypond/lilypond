\score{
	<
		  \context Staff \notes\relative c'{ 
			  c d e f
			  \repeat semi 2 { g a b c }
			  \alternative { { c b a g } { f e d c } }
		  }
		  \context Lyrics \lyrics {
			  De eer- ste < { maat } { moet } >
			  \repeat fold 2 { }
			  \alternative {
				  { en dan twee keer } 
				  { een koe- plet _ } 
			  }
			  en dan nog dit er ach- ter aan
		  }
	>
}
