foo = \notes { \context Voice { \relative c'' {
\repeat volta 3 { e c d e } \break
\repeat volta 3 { e c d e }
}
}
}
\score {
\context PianoStaff <
\context Staff = one \foo
\context Staff = two \foo
>
\midi{}
\paper{}
}
foo = \notes { \context Voice { \relative c'' {
\repeat volta 3 { e c d e } \break
\repeat volta 3 { e c d e }
}
}
}
\score {
\context PianoStaff <
\context Staff = one \foo
\context Staff = two \foo
>
	\paper {	linewidth = 17.0\cm;
			\translator  {
				\OrchestralScoreContext
				minVerticalAlign = 2.5*\staffheight;
			}
			\translator {
				\StaffContext
				\consists Instrument_name_engraver;
			}
	}
\midi{}
}
