\version "1.7.18"
% MERGED: stuff from follow-break.ly to follow-thread.ly.  DELETE NOW

\header{
%	texidoc = "@cindex followVoice Break
%followVoice: connect note heads with line when thread switches staff. " }
texidoc = "MERGED with follow-thread.  DELETE."}

\score{
	\context PianoStaff <
		\context Staff=one \notes\relative c''{
			\context Thread {
         a1 \break
	    	\translator Staff=two
	    	a,
			}
	}
	\context Staff=two { \clef bass \skip 1*2 }
    >
    \paper{
		raggedright = ##t
        linewidth = 70.\mm
	\translator {
	    \ScoreContext
	    followVoice = ##t
	}
    }
}
%% new-chords-done %%
