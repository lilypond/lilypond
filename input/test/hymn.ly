\version "1.7.18"
%  We have other examples of partcombine.  Candidate for delete.  -gp
%  leave it for now; I need to check some things.
% FIXMEsoon -gp
%{
Jan:
> Why remove this?  Do we have another hymn with part-combiner?  Maybe
> move to template?
%}

\header { texidoc = "@cindex Hymn
You can combine two parts on the same staff. " }

\score{
	\context Staff <
		\time 4/4
		\context Voice=one \partcombine Voice
			\context Thread=one \notes\relative c'' {
				a4 c4.(g8-) a4 |
				g4 e' g(f-) | 
				b, a c2
			}
			\context Thread=two \notes\relative c'' {
				g4 e4.(d8-) c4 |
				g'4 c, e(f-) |
				d2 a
			}
	>
	\paper{
		linewidth=140.\mm
		\translator {
			\VoiceContext
			soloADue = ##f
		}
	}
}


