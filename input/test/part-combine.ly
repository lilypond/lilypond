\score{
	\context Staff = flauti <
		\time 4/4;

		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c''
				{
					c4 d e f
					b,4 d c d
					r2 e4 f
					c4 d e f
					c4 r e f
					c4 r e f
					c4 r a r
					a a r a
					a2 \property Voice.soloADue = ##f a
				}
			\context Thread=two \notes\relative c''
				{
					g4 b d f
					r2 c4 d
					a c c d
					a4. b8 c4 d
					c r e r
					r2 s2
					a,4 r a r
					a r r a
					a2 \property Voice.soloADue = ##f a
				}
		>
	\paper{
		linewidth = 140.\mm;
		\translator{
			\ThreadContext
			\consists Rest_engraver;
		}
		\translator{
			\VoiceContext
			\remove Rest_engraver;
		}
	}
}
