
% add cue notes context
\paper {
	CueVoiceContext = \translator {
            \VoiceContext
            \name CueVoice;
            basicNoteHeadProperties \push #'font-relative-size = #-1
            basicStemProperties \push #'font-relative-size = #-1
            basicBeamProperties \push #'font-relative-size = #-1
            basicTextScriptProperties \push #'font-relative-size = #-1
            basicSlurProperties \push #'font-relative-size = #-1
            basicLocalKeyProperties \push #'font-relative-size = #-1
	};
	\translator{ \CueVoiceContext }
	StaffContext = \translator{\StaffContext
		\accepts "CueVoice";
	}; 
}

\paper {
    StaffContext = \translator{
       \StaffContext
       \remove "Time_signature_engraver";
    };
    \translator { \StaffContext }   
    \translator{
      \VoiceContext
      \remove "Auto_beam_engraver";
     }
     \translator {
         \ScoreContext
         barScriptPadding = #2.0
         markScriptPadding = #4.0
         barNumberScriptPadding = #15
     }
}

\score{
  \notes \relative c' \context Voice
      {
        c4 c4 g'2 
	 \context CueVoice { r2 [f16 f f f] [a f f f] }
	c4 c4 g'2
      }
}

