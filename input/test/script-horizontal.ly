
\score {

	\notes \context Voice <
		\context Thread = TA { c'4_1 }
		\context Thread = TB { \property Thread.scriptHorizontal = "1"
		e'4-2-\trill }
		\context Thread = TC { g'4^4 }
	>
	
	\paper { \translator {
		\VoiceContext
		\remove Script_engraver;
		\remove Text_engraver;
		
		}
		\translator {
		\ThreadContext
		\consists Script_engraver;
		\consists Text_engraver;		
		}
	}
}
