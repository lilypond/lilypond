\version "2.3.2"

\header{
texidoc="
Adding a @code{Bar_engraver} to the Lyrics context makes sure that
lyrics do not collide with barlines.
"
}

\score {
	\context StaffGroup <<
	\notes \context Staff {
	        b1 \bar "|:" b1 \bar ":|"
	}
	\lyrics <<
	 	\context LyricsWithBars {
%		        thisContextHasBarEngraver1  added
		        ThisContextCertainlyHasBarEngraverAddedButThereHasBeenSomethingFunnyBefore1.  Here.
		}
		\context Lyrics {
		        this4 one has no BarEngraverAddedToContext1
		}
	>>
	\notes \new Staff { b1 b1 }
	>>
	\paper {
		raggedright = ##t
		\context {
			\Score
			\accepts "LyricsWithBars"
		}
		\context {
			\Lyrics
			\consists "Bar_engraver"
			\name "LyricsWithBars"
		}
	}
}

