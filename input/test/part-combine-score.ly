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
			\context StaffCombineVoice=one \notes\relative c''
			%\context StaffCombineThread=one \notes\relative c''
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
			\context StaffCombineVoice=two \notes\relative c''
			%\context StaffCombineThread=two \notes\relative c''
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
			aDueText = #""
			splitInterval = #'(1 . 0)
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
