\version "1.3.96"
chord = \notes\transpose c''\chords{
   c1
   c:m
   c:m5-
   c:m5-.7-
   c:7+
   c:m5-.7
   c:5-.7+
   c:m7
   c:7
   d
   d/a
   d/+gis
}

\score{
    <
	    \context ChordNames \chord
	    \context Staff \chord
    >
    \paper{
    	linewidth = -1.0;
        	\translator { 
			\ChordNamesContext
			ChordName \push #'word-space = #1 
		}
    }
}

