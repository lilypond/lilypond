\version "2.1.22"

% possible rename to lyric-repeat or repeat-lyric.

\header{ texidoc = "@cindex Repeat Lyrics
You can use alternate lyrics as well as alternate notes for repeats. "
}

\score{
	<<
		  \context Staff \notes\relative c'{ 
			  c d e f
			  \repeat "volta" 2 { g a b c }
			  \alternative { { c b a g } { f e d c } }
		  }
		  \context Lyrics \lyrics {
			  De eer- ste << { maat } { moet } >>
			  \repeat fold 2 { }
			  \alternative {
				  { en dan twee keer } 
				  { een koe- plet _ } 
			  }
			  en dan nog dit er ach- ter aan
		  }
	>>
	\paper{raggedright=##t}
}

