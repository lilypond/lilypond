\version "1.7.18"
% MERGED: stuff from follow-break.ly to follow-thread.ly

% followVoice: connect note heads with line when thread switches staff 
\header{ texidoc="@cindex followVoice Thread
Theads can be traced automagically when they switch staves by setting
property @code{followVoice}. " }


\score {
	\notes\relative c {
	\context PianoStaff=ps <
		\property PianoStaff.followVoice = ##t
		\context Staff=one \context Voice {
			\property Voice.VoiceFollower \set #'type = #'dashed-line
			c'1
			\translator Staff=two
			b2 a
% these lines from follow-break.ly:
			\translator Staff=one
			a1
		    }
		\context Staff=two {\clef bass \skip 1*3 }
		>
	}

  \paper { raggedright = ##t }  
}
%% new-chords-done %%
