\version "1.0.12";

%{
Would this be acceptable/good enough/convenient for entry?

   Convention/Standard    Logical/Lily(?)
   
   C#                     cis
   Cb                     ces
   Cm/Cmin                c3-     
   Caug                   c5+
   Cdim                   c5-
   Cmaj7                  c7
   C7                     c7-
   Csus/Csus4             c4^3
%}

scales = \notes\transpose c''\chords{
		*c *g *d *a *e *b *fis
		*c *f *bes *es *as *des *ges
                *c6 *c7 *c9 *c11 *c13
	}

\score{
	<
		\type ChordNames \scales
		\type Staff \scales
	>
}
