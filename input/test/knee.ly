\version "1.5.68"

\score{
	\notes\relative c'{
	        \property Voice.Beam \override #'auto-knee-gap = ##f
		[c16 \stemDown c'' \stemBoth c,, d]
		\stemDown [d'' a \stemBoth c,, g,]
		\stemUp [g c' \stemBoth a'' d']
	}
}
