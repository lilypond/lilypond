%
% Test new font selection and scm text markup
%

\score{
	\notes\relative c''{
		a-"text"
		b-\textscript #"texta"
		c-\textscript #'(bold "textb")
		d-\textscript #'(lines "een" "twee" "drie")
		e-\textscript #'(lines (bold "een") 
		  (rows "en" "dat" "is" ((family . "number") "2"))
		  (italic "drie"))
		f-\textscript #'(finger "3")
		g-\textscript #'(music (named "noteheads-2"))
	}
	\paper{
		linewidth = -1.\mm;
		\translator{
			\ScoreContext
			TextScript \push #'style-sheet = #'paper16
			TextScript \push #'font-family = #'roman
			TextScript \pop #'no-spacing-rods
			TextScript \push #'direction = #1
		}
	}
}
