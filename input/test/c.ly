\version "1.0.12";

%{
Would this be acceptable/good enough/convenient for entry?

   Convention/Standard    Lily
   
   C#                     cis
   Cb                     ces
   Cm; Cmin               c3-; c m; c min
   Caug                   c5+; c aug;
   Cdim                   c5-; c dim
   Cmaj7                  c7+; c maj
   C7                     c7
   Csus; Csus4            c4; c sus
%}

scales = \notes\transpose c''\chords{
		<c1 e g>
		@1c m @c min @4c dim @c aug @c sus @c maj
                @1c6 @4c7 @c9 @c11 @c13
		@1c @g @d @a @e @b @fis
                @1c @f @bes @es @as @des @ges
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
	}

\score{
	<
		\type ChordNames \scales
		\type Staff < \scales \keys >
	>
}
