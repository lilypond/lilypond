\version "1.7.18"
% I don't understand the point of this one.  Demonstrates
% wierd beamings?  I'd say delete it or move to regression.
\header{ texidoc="DELETE or MOVE TO REGRESSION
"}
\score{
	\notes\relative c'{
		\stemUp
		 f8-[ a' g f]
		 c8-[ g'16 f]
		 c8-[ e16 d]
		 a16-[ b c d]
		 d16-[ c b a]
		\stemDown
		 c16-[ b a g]
		 g16-[ a b c]
	}
	\paper{
		raggedright = ##t
	}
}
%% new-chords-done %%
