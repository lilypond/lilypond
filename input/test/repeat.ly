\version "2.3.4"

% possible rename to lyric-repeat or repeat-lyric.

\header{ texidoc = "@cindex Repeat Lyrics
Alternate lyrics can be used, as well as alternate notes for repeats. "
}

%% Syntax << { aaa } { bbb } >> seems not to work in lyrics. 
%% The notes are lyrics do not match syntactically here. - HJJ

\score{
	<<
		  \context Staff \relative c'{ 
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

