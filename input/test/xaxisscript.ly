
\score {
\notes {\property Thread.scriptHorizontal = "1"
	c4-4
	}
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
