\header{
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures =	 "chord inversions";
}

inversions = \notes\transpose c''\chords{
	c1 c-sus c-6 c/e c/g c/d
	% now try to find chords with inversions
	\property Score.chordInversion = 1 
	c1 c-sus c-6 c/e c/g c/d
}

\score{
	<
		\type ChordNames \inversions
		\type Staff \inversions
	>
}
