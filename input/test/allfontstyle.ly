


\score {  \notes
	\relative c'' \context Staff {
		\repeat "semi" 2 { \time 4/4; c4^"foo" }
		 \alternative { d-4 e-\fermata  } \grace { c16 } f-\ff
	}
	\paper { linewidth = -1.0; }
}
