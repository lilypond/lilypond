

\header{
texidoc = "Grace notes may separate beams, underneath normal beams." 
}

%
% TODO: add this to standard lilypond
%

\score{
\notes\context Voice{
        \time 6/8
        \clef violin
	\key es \major
        [a'8 ( \context GraceVoice \grace { [bes'16 a'16] }  g'8 ) a'8] d''4 c''8 |
}
\paper { \translator { \VoiceContext
		       \accepts GraceVoice
	 }
	 \translator { \VoiceContext \name GraceVoice }
	 linewidth = -1. 
    }
}

