\paper {

	textheight = 295.0\mm;
	linewidth = 180.0\mm;

	\translator{ \HaraKiriStaffContext }
	%
	% The Voice combine hierarchy
	%
	\translator{
		\ThreadContext
		\name "VoiceCombineThread";
		\consists "Rest_engraver";
		\remove "Thread_devnull_engraver";
		\consists "A2_devnull_engraver";
	}
	\translator{
		\VoiceContext
		\name "VoiceCombineVoice";
		soloText = #"I."
		soloIIText = #"II."
		\remove "Rest_engraver";
		\accepts "VoiceCombineThread";
		\consists "A2_devnull_engraver";
		\remove "Voice_devnull_engraver";
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
		\remove "Thread_devnull_engraver";
%%		\consists "A2_devnull_engraver";
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
		\remove "Voice_devnull_engraver";

		\remove "Piano_pedal_engraver";
		\remove "Script_engraver";
		\remove "Script_column_engraver";
		\remove "Rhythmic_column_engraver";
		\remove "Slur_engraver";
		\remove "Tie_engraver";

		\remove "Voice_devnull_engraver";
%%		\consists "Thread_devnull_engraver";
		\consists "A2_devnull_engraver";
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
%%		\consists "Voice_devnull_engraver";
%%		\consists "A2_devnull_engraver";

		soloADue = ##f

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
	\translator{ \HaraKiriStaffContext }

	\translator {
		\ScoreContext
		\accepts "VoiceCombineStaff";
		\accepts "StaffCombineStaff";
		skipBars = ##t 

		barScriptPadding = #2.0 % dimension \pt
		markScriptPadding = #4.0

		%% urg: in pt?
		barNumberScriptPadding = #15
		%% URG: this changes dynamics too
		%%textStyle = #"italic"
		timeSignatureStyle = #"C"
		instrumentScriptPadding = #60  %% urg, this is in pt
		instrScriptPadding = #40 %% urg, this is in pt
		marginScriptHorizontalAlignment = #1
		maximumRestCount = #1
	}
}

