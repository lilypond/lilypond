
\version "1.3.117";


% add cue notes context
\paper {
	CueVoiceContext = \translator {
            \VoiceContext
            \name CueVoice;
            basicNoteHeadProperties \override #'font-relative-size = #-1
            basicStemProperties \override #'font-relative-size = #-1
            basicBeamProperties \override #'font-relative-size = #-1
            basicTextScriptProperties \override #'font-relative-size = #-1
            basicSlurProperties \override #'font-relative-size = #-1
            basicLocalKeyProperties \override #'font-relative-size = #-1
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

