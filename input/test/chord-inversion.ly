\header{
enteredby =	 "jcn";
copyright =	 "public domain";
TestedFeatures =	 "chord inversions";
}

inversions = \notes\transpose c''\chords{
	% inversions
	c1 c:sus c:6 c/e c/g
	c/d % this triggers a warning: no 'd' in chord of c

	% added bass 
	c/+e c/+c c/+b
}

\score{
	<
		\context ChordNames \inversions
		\context Staff \inversions
	>
}

\version "1.3.96"; 
