\version "1.3.59";

%{
Would this be acceptable/good enough/convenient for entry?

   Convention/Standard    Lily
   
   C#                     cis
   Cb                     ces
   Cm; Cmin               c:3-; c:m; c:min
   Caug                   c:5+; c:aug;
   Cdim                   c:3-.5-; c:dim
   Cmaj7                  c:7+; c:maj
   C7                     c:7
   Csus; Csus4            c:4; c:sus

%}

scales = \notes \transpose c'' \chords{
	\property ChordNames.drarnChords=##t
		%c1:m \break c1:m
		c1:m c1:m
}

%\include "paper-as9.ly";
\score{
	<
		\context ChordNames \scales
		\context Staff \scales
	>
	\paper{
%		\paper_as_nine
		\translator {
			\ChordNameContext
			}
	}
}
