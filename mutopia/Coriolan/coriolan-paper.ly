\version "1.3.120"
\paper{
	\paperSixteen

	%textheight = 290.0\mm;
	%linewidth = 195.0\mm;
	textheight = 285.0\mm;
	linewidth = 190.0\mm;

	\translator{ \HaraKiriStaffContext }
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
		soloText = #"I."
		soloIIText = #"II."
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
	}
	\translator{
		\VoiceContext
		\name "StaffCombineVoice";
		\accepts "StaffCombineThread";
		\consists "Thread_devnull_engraver";
	}
	\translator {
		\HaraKiriStaffContext
		\name "StaffCombineStaff";
		\accepts "StaffCombineVoice";

		soloADue = ##t
		soloText = #""
		soloIIText = #""
		% This is non-conventional, but currently it is
		% the only way to tell the difference.
		aDueText = #"\\`a2"
		splitInterval = #'(1 . 0)
		changeMoment = #`(,(make-moment 1 1) . ,(make-moment 1 1))
	}
	\translator {
		\StaffGroupContext
		\accepts "VoiceCombineStaff";
		\accepts "StaffCombineStaff";
	}
	\translator{ \HaraKiriStaffContext }

	\translator {
		%\ScoreContext
		\OrchestralScoreContext
		\accepts "VoiceCombineStaff";
		\accepts "StaffCombineStaff";
		skipBars = ##t 

		barScriptPadding = #2.0 % dimension \pt
		markScriptPadding = #4.0

		%% urg: in pt?
		barNumberScriptPadding = #15
		maximumRestCount = #1
	}
}
