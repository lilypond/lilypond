\version "1.7.18"

% followVoice: connect note heads with line when thread switches staff 
\header{ texidoc="@cindex followVoice Thread
Theads can be traced automagically when they switch staves by setting
property @code{followVoice}. " }


fragment = \notes {
  \context PianoStaff <
    \property PianoStaff.followVoice = ##t
    \property Voice.VoiceFollower \set #'type = #'dashed-line
    \context Staff \context Voice {
      c'1
      \translator Staff=two
      b2 a
    }
    \context Staff=two {\clef bass \skip 1*2}
  >
}

\paper { raggedright = ##t} 

\score {
  \notes\relative c \fragment
  \paper { }  
}
%% new-chords-done %%
