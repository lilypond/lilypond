\score{
	<
		\type Staff = o \notes\relative c,,, { \stemdown [c8 c c c] }
		\type Lyrics \lyrics{ BLA bla }
		\type Staff = t \notes\relative c''''' { \stemup [c8 c c c] }
	>
	\paper{
		castingalgorithm = \Wordwrap;
	}
}
