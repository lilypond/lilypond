\header{
texidoc="
Adding a @code{Bar_engraver} to the LyricsVoice context makes sure that
lyrics don't collide with barlines.
";
}

\score {
	\context StaffGroup <
	\notes \context Staff {
	        b1 b1 \bar "|.";
	}
	\lyrics\context Lyrics <
	 	\context LyricsVoiceWithBars {
		        thisContextHasSpanBarEngraver1 added
		}
		\context LyricsVoice {
		        this4 one has no SpanBarEngraverAddedToContext1
		}
	>
	\notes \context Staff = SB { b1 b1 }
	>
	\paper {
		linewidth = -1.0\cm;
		\translator {
			\LyricsContext
			\consists "Span_bar_engraver";
			\accepts "LyricsVoiceWithBars";
		}
		\translator {
			\LyricsVoiceContext
			\name "LyricsVoiceWithBars";
		}
		\translator {
			\LyricsVoiceContext
		}
	}
}
