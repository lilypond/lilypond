\version "1.0.16";

%{
Would this be acceptable/good enough/convenient for entry?

   Convention/Standard    Lily
   
   C#                     cis
   Cb                     ces
   Cm; Cmin               c-3-; c-m; c-min
   Caug                   c-5+; c-aug;
   Cdim                   c-5-; c-dim
   Cmaj7                  c-7+; c-maj
   C7                     c-7
   Csus; Csus4            c-4; c-sus

%}

scales = \notes \transpose c'' \chords{
		%<c1 e g>
		c1-m c-min c4-dim c-aug c-sus c-maj
                c1-6 c4-7 c-9 c-11 c-13
		c1-7^5 c-13^5.7.9.11
		% c1-7^5 c-13^5
		c1 g d a e b fis
                c1 f bes es as des ges
		% wierd, multiple -add, ^sub
		c-7+.9-^3.5
		% long
		c\breve c\longa
	}

keys = \notes{
                s1
                s1 s1 s1
                s1 s1
                s1
                \key g; s1
                \key d; s1 
                \key a; s1 
                \key e; s1 
                \key b; s1 
                \key fis; s1
                \key c; s1 
                \key f; s1 
                \key bes; s1
                \key es; s1
                \key as; s1
                \key des; s1
                \key ges; s1
                \key c; s1*2
                \key c; s1*6
	}

\score{
	<
		\context ChordNames \scales
		\context Staff < \scales \keys >
	>
}
