
soprano = \notes \relative c''  {
	fis8 g  g8.-\trill fis32 g a8 d, r a
	b4 ~ \times 2/3 { [b16 cis d] } \times 2/3 { [e fis g] }
	a,4 ~ \times 2/3 { [a16 b cis] } \times 2/3 { [d e fis] }
	a8 g16. fis32 g8.-\trill fis16 fis4
}

bass = \notes \relative c {
	\property Voice.verticalDirection = \down
	d4 e fis r8 fis | g4. g8 fis4. fis8 e8 e a a d,4
}

tenor = \notes \relative c' {
	\property Voice.verticalDirection = \up
	r8
	d4 cis8 r d16. cis32 d4 | r8 e16. dis32 e4 ~ e8 d16. cis32 d4 ~ |
	[d8 d8] [cis cis] d4
}


\score {
	\context PianoStaff \notes <
		\context Staff = top { \key D; \time 4/4;
			\soprano
		}
		\context Staff = bottom < \key D; \time 4/4; \clef bass;
			\context Voice = up  { \tenor } 
			\context Voice = down  { \bass }
		>
		
	>
	\midi{}
	\paper {}
}

	
	 
