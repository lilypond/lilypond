\score{
	<
	\context VoiceCombineStaff = flauti <
		\time 4/4;

		\context VoiceCombineThread=one \skip 1*9;
		\context VoiceCombineThread=two \skip 1*9;

		\context VoiceCombineVoice=one \partcombine VoiceCombineVoice
			\context VoiceCombineThread=one \notes\relative c''
				{
					c4 d e f
					b,4 d c d
					r2 e4 f
					\break
					c4 d e f
					c4 r e f
					c4 r e f
					\break
					c4 r a r
					a a r a
					a2 \property VoiceCombineThread.soloADue = ##f a
				}
			\context VoiceCombineThread=two \notes\relative c''
				{
					g4 b d f
					r2 c4 d
					a c c d
					a4. b8 c4 d
					c r e r
					r2 s2
					a,4 r a r
					a r r a
					a2 \property VoiceCombineThread.soloADue = ##f a
				}
		>
	\context PianoStaff <
		\context StaffCombineStaff=one \skip 1*9;
		\context StaffCombineStaff=two \skip 1*9;
		\context StaffCombineStaff=one \partcombine StaffCombineStaff
			\context StaffCombineThread=one \notes\relative c''
				{
					c4 d e f
					c d e f
					c d e f
					c d e f
					c d e f
					c d e f
					c4 d e f
					a8 a a a
					b b b b
					d1
				}
			\context StaffCombineThread=two \notes\relative c''
				{
					c4 d e f
					c d e f
					c d e f
					c2 e2
					c4 d e f
					c2 e2
					c,4 d e f
					a8 a a a
					b b b b
					b1
				}
		>
	>
	\paper{
		%
		% The Voice combine hierarchy
		%
		\translator{
			\ThreadContext
			\name "VoiceCombineThread";
			\consists "Rest_engraver";
		}
		\translator{
			\VoiceContext
			\name "VoiceCombineVoice";
			\remove "Rest_engraver";
			\accepts "VoiceCombineThread";
		}
		\translator{
			\HaraKiriStaffContext
			\consists "Mark_engraver";
			\name "VoiceCombineStaff";
			\accepts "VoiceCombineVoice";
		}

		%
		% The Staff combine hierarchy
		%
		\translator{
			\ThreadContext
			\name "StaffCombineThread";
%%
%%			\remove "A2_devnull_engraver";
%%			\remove "Note_heads_engraver";
		}
		\translator{
			\VoiceContext
			\name "StaffCombineVoice";
			\accepts "StaffCombineThread";

			\remove "Rest_engraver";
			\remove "Dot_column_engraver";
			\remove "Stem_engraver";
			\remove "Beam_engraver";
			\remove "Auto_beam_engraver";
			%\include "auto-beam-settings.ly";

			\remove "Chord_tremolo_engraver";
			\remove "Melisma_engraver";
			\remove "Text_engraver";
			\remove "A2_engraver";

			\remove "Piano_pedal_engraver";
			\remove "Script_engraver";
			\remove "Script_column_engraver";
			\remove "Rhythmic_column_engraver";
			\remove "Slur_engraver";
			\remove "Tie_engraver";

%%			\consists "A2_devnull_engraver";
%%			\consists "Note_heads_engraver";
		}
		\translator {
			\HaraKiriStaffContext
			\name "StaffCombineStaff";
			\accepts "StaffCombineVoice";

			\consists "Rest_engraver";
			\consists "Dot_column_engraver";
			\consists "Stem_engraver";
			\consists "Beam_engraver";
			\consists "Auto_beam_engraver";
			\include "auto-beam-settings.ly";

			\consists "Chord_tremolo_engraver";
			\consists "Melisma_engraver";
			\consists "Text_engraver";
			\consists "A2_engraver";

			soloADue = ##f
			%soloADue = ##t

			\consists "Piano_pedal_engraver";
			\consists "Script_engraver";
			\consists "Script_column_engraver";
			\consists "Rhythmic_column_engraver";
			\consists "Slur_engraver";
			\consists "Tie_engraver";
		}
		\translator {
			\StaffGroupContext
			\accepts "VoiceCombineStaff";
			\accepts "StaffCombineStaff";
		}
		\translator {
			\ScoreContext
			\accepts "VoiceCombineStaff";
			\accepts "StaffCombineStaff";
		}
%		linewidth = 40.\mm;
	}

	\midi{ 
		\tempo 4 = 150; 

		\translator {
			\ThreadContext
			\name "VoiceCombineThread";
		}
		\translator {
			\VoiceContext
			\name "VoiceCombineVoice";
			\accepts "VoiceCombineThread";
		}
		\translator {
			\StaffContext
			\name "VoiceCombineStaff";
			\accepts "VoiceCombineVoice";
		}

		\translator {
			\ThreadContext
			\name "StaffCombineThread";
		}
		\translator {
			\VoiceContext
			\name "StaffCombineVoice";
			\accepts "StaffCombineThread";
		}
		\translator {
			\StaffContext
			\name "StaffCombineStaff";
			\accepts "StaffCombineVoice";
		}
		\translator {
			\ScoreContext
			\accepts "VoiceCombineStaff";
			\accepts "StaffCombineStaff";
		}
	}
}
