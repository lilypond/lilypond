\score{
	\addlyrics
		\context Staff {
			\notes\relative c''{ c () d r e f }
			\notes\relative c''{ c ~ c r c c }
		}
		{
			\context Lyrics \lyrics { foo __ bar baz }
			\context Lyrics \lyrics { foo __ bar baz }
		}
	\paper{
		\translator{
			\VoiceContext
			automaticMelismas=1;
		}
	}
}
