\version "2.1.19"

% possible rename to lyric-repeat or repeat-lyric.

\header{ texidoc = "@cindex Repeat LyricsVoice
You can use alternate lyrics as well as alternate notes for repeats. "
}

\score{
	<<
		  \context Staff \notes\relative c'{ 
			  c d e f
			  \repeat "volta" 2 { g a b c }
			  \alternative { { c b a g } { f e d c } }
		  }
		  \context LyricsVoice \lyrics {
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

