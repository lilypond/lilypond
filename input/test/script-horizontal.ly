\version "1.3.146"
%{

Please don't use this Scheme hacking unless you have a good reason.

%}
	
\score {
	\notes \context Voice <
		\context Thread = TA { c'4_1 }
		\context Thread = TB {
			\property Thread.scriptHorizontal = ##t
			\property Thread.TextScript \set #'extra-offset = #'(-0.5 . -0.5)
			\property Thread.Script \set #'extra-offset = #'(2.25 . -0.5)
		e'4-2^\prall }
		\context Thread = TC { g'4^4 }
	>
	
	\paper { \translator {
		\VoiceContext
		\remove Script_engraver
		\remove Text_engraver
		}
		\translator {
		\ThreadContext
		\consists Script_engraver
		\consists Text_engraver		
		}
	}
}
