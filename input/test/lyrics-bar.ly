
\score {

	\context StaffGroup <
	 \notes \context Staff { b1 b1 \bar "|."; }
	 \lyrics\context Lyrics <
	 	\context BarLV  { thisContextHasBarEngraver1 added }
		 \context LyricVoice { this4 one has no barEngraverAddedToContext1 }
		>
	 \notes \context Staff = SB { b1 b1 }

	 	>

	\paper {
		linewidth = -1.0\cm;
		\translator {
			\LyricsContext
			\consists "Bar_engraver";
			\accepts "BarLV";
		}
		\translator {
			\LyricsVoiceContext
			\consists "Bar_engraver";
			\name "BarLV";
		}
		\translator {
			\LyricsVoiceContext
		}
	}
}
