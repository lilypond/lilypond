
\version "1.3.110";

%
% Test new font selection and scm text markup
%

\score{
	\notes\relative c''{
		% put me in an engraver
		a1-#'(rows (music "noteheads-2" ((kern . -0.1) "flags-stem") ((kern . -0.1) ((raise . 3.5) "flags-u3"))) " = 64")
		}
	\paper{
		linewidth = -1.\mm;
		\translator{
			\ScoreContext
			TextScript \override #'font-shape = #'upright
			TextScript \override #'direction = #1
		}
	}
}
