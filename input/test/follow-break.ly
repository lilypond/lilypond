\version "1.7.18"
% renamed from broken-thread-line.ly to follow-break.ly  -gp
% possible merge with follow-thread, or regression, or delete.  -gp
\header{
	texidoc = "@cindex followVoice Break
followVoice: connect note heads with line when thread switches staff.
" }
\score{
    \context PianoStaff <
        \context Staff=one \notes\relative c''{
	    \context Thread
            a1 \break
	    \translator Staff=two
	    a,

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
