\version "2.1.22"

\header{
texidoc="
Adding a @code{Bar_engraver} to the Lyrics context makes sure that
lyrics don't collide with barlines.
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
		\translator {
			\ScoreContext
			\accepts "LyricsWithBars"
		}
		\translator {
			\LyricsContext
			\consists "Bar_engraver"
			\name "LyricsWithBars"
		}
	}
}

