\version "2.3.22"

\header{
texidoc="
Adding a @code{Bar_engraver} to the Lyrics context makes sure that
lyrics do not collide with barlines.
"
}

\score {
	\context StaffGroup <<
	 \context Staff=foo {
	        b1 \bar "|:" b1 \bar ":|"
	}
	 	\context LyricsWithBars \lyricmode {
%		        thisContextHasBarEngraver1  added
		        ThisContextCertainlyHasBarEngraverAddedButThereHasBeenSomethingFunnyBefore1.  HereThereWhere.
		}
		\context Lyrics \lyricmode {
		        this4 one has no BarEngraverAddedToContext1
		}
	 \context Staff=bar { b1 b1 }
	>>
	\layout {
		raggedright = ##t
		\context {
			\StaffGroup
			\accepts "LyricsWithBars"
		}
		\context {
			\Lyrics
			\consists "Bar_engraver"
			\name "LyricsWithBars"
		}
	}
}

