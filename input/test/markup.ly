%
% Test new font selection and scm text markup
%

\score{
	\notes\relative c''{
		\stemUp
		a-"text"
		b-\textscript #"texta"
		c-\textscript #'(bold "textb")

		d-\textscript #'(lines "one" "two" "three")
		e-\textscript #'(lines (bold "one") 
		  (rows "and" "there" "is" ((family . "number") "2"))
		  (italic "three"))
		f-\textscript #'(finger "3")
		g-\textscript #'(music (named "noteheads-2"))
	}
	\paper{
		linewidth = -1.\mm;
		\translator{
			\ScoreContext
			TextScript \push #'font-family = #'roman
			TextScript \pop #'no-spacing-rods
			TextScript \push #'direction = #1
		}
	}
}
