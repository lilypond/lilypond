\include "paper23.ly"

\score {
   \notes \relative c'' {
	\clef "treble"
	\time 4/4
	\partial 4
	\property Voice.Slur \override
		#'attachment-offset = #'((-0.5 . 0.75) . (0 . 0.75))
	\property Voice.DynamicText \override #'padding = #-2
	\property Voice.DynamicLineSpanner \override #'padding = #2
	[e,8-"."-\pp-\cr-\upbow ( )f-"."-\rc] |
	ges4-\decr ( ) f8-\rced r8 r2 \bar "||"
	

   }
   \paper {
   linewidth=9.0\cm
   }
}
