\version "1.3.120"
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
