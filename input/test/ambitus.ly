\version "1.5.68"

upper = \notes \relative c {
	\clef "treble"
	\key c \minor
	as'' c e bes f cis d e f g f e d f d e
	f d e e d f d e e d f d e e d f d e
	f d e e d f d e e d f d e e d f d e
}

lower = \notes \relative c {
	\clef "treble"
	\key e \major
	e'2 b4 g a c es fis a cis b a g f e d
	f e d e f g f e d e f g f e d e f g
	f e d e f g f e d e f g f e d e f g
}

\score { \context ChoirStaff {
	<
		\context Staff = one { \upper }
		\context Staff = three { \lower }
	> }
	\paper {
	       \translator {
			\ScoreContext
			breakAlignOrder = #'(
				instrument-name
				left-edge
				ambitus
				span-bar
				breathing-sign
				clef
				key-signature
				staff-bar
				time-signature
				custos
			)
		}
		\translator {
			\VoiceContext
%%			\consists Ambitus_engraver
		}
	}
}
