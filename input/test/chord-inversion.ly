\header{
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures =	 "chord inversions";
}

inversions = \notes\transpose c''\chords{
	% inversions ignored here
	c1 c-sus c-6 c/e c/g c/d
	% now try to find chords with inversions
	\property Score.chordInversion = 1 
	c1 c-sus c-6 
	c/e
	c/g
	c/d % this triggers a warning: no 'd' in chord of c
}

\score{
	<
		\context ChordNames \inversions
		\context Staff \inversions
	>
}

\version "1.1.52"; 
