\version "2.3.4"
% MERGED: stuff from follow-break.ly to follow-thread.ly

% followVoice: connect note heads with line when thread switches staff 
\header{ texidoc="@cindex followVoice Voice
Voices can be traced automatically when they switch staves by setting
@code{followVoice}. " }


\score {
	\relative c {
	\new PianoStaff <<
		\set PianoStaff.followVoice = ##t
		\context Staff=one \context Voice {
			\override VoiceFollower  #'style = #'dashed-line
			c'1
			\change Staff=two
			b2 a
% these lines from follow-break.ly:
			\change Staff=one
			a1
		    }
		\context Staff=two {\clef bass \skip 1*3 }
		>>
	}

  \paper { raggedright = ##t }  
}

