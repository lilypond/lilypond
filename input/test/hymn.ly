\score{
	\context Staff <
		\context Voice=one { \skip 1; }
		\context Voice=two { \skip 1; }

		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c'' {
				%a4 c4.()g8 a4 |
				g4 e' g()f | 
				b, a c2
			}
			\context Thread=two \notes\relative c'' {
				%g4 e4.()d8 c4 |
				g4 c, e()f |
				d2 a
			}
	>
	\paper{
		linewidth=140.\mm;
		\translator {
			\VoiceContext
			soloADue = ##f
			%\remove Slur_engraver;
			\consists Slur_engraver;
		}
%		\translator {
%			\ThreadContext
%			\consists Slur_engraver;
%		}
	}
}

