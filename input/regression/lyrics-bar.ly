\version "2.3.22"

\header{
texidoc="
Adding a @code{Bar_engraver} to the Lyrics context makes sure that
lyrics do not collide with barlines.
"
}

\score {
	\context StaffGroup <<
	 \context Staff {
	        b1 \bar "|:" b1 \bar ":|"
	}
	\lyricmode <<
	 	\context LyricsWithBars {
%		        thisContextHasBarEngraver1  added
		        ThisContextCertainlyHasBarEngraverAddedButThereHasBeenSomethingFunnyBefore1.  Here.
		}
		\context Lyrics {
		        this4 one has no BarEngraverAddedToContext1
		}
	>>
	 \new Staff { b1 b1 }
	>>
	\layout {
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

