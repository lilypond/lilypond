\score{
	\context PianoStaff <
		\context Staff=one \skip 1*5;
		\context Staff=two \skip 1*5;
		\context Staff=one \partcombine Staff
			\context Voice=one \notes\relative c''
				{
					c4 d e f\break
					c d e f\break
					c d e f
					c4 d e f\break
					a8 a a a b b b b
				}
			\context Voice=two \notes\relative c''
				{
					c2 e2
					c4 d e f
					c2 e2
					c,4 d e f
					a8 a a a b b b b
				}
		>
	\paper{
		%\translator { \HaraKiriStaffContext }
		\translator {
			\HaraKiriStaffContext

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

			\consists "Piano_pedal_engraver";
			\consists "Script_engraver";
			\consists "Script_column_engraver";
			\consists "Rhythmic_column_engraver";
			\consists "Slur_engraver";
			\consists "Tie_engraver";
		}
		\translator{
			\VoiceContext

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

			\consists "A2_devnull_engraver";
			\consists "Note_heads_engraver";
		}
		\translator{
			\ThreadContext
			\remove "A2_devnull_engraver";
			\remove "Note_heads_engraver";
		}
		linewidth = 40.\mm;
	}
}
