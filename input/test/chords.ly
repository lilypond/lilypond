\version "1.5.68"


%{
Would this be acceptable/good enough/convenient for entry?

   Convention/Standard    Lily
   
   C#                     cis
   Cb                     ces
   Cm Cmin               c:3- c:m c:min
   Caug                   c:5+ c:aug
   Cdim                   c:3-.5- c:dim
   Cmaj7                  c:7+ c:maj
   C7                     c:7
   Csus Csus4            c:4 c:sus

%}

scales =  \notes \transpose c'' \chords{
		%<c1 e g>
		c1:m c:min c4:dim c:aug c:sus c:maj
                c1:6 c4:7 c:9 c:11 c:13
		c:m7 c:m.sus c:m7.sus
		c4:dim7 c:dim9 c2:7^5 
		c:13^5.7.9.11
		% c1:7^5 c:13^5
		c1 g d a e b fis
                c1 f bes es as des ges
		% wierd, multiple :add, ^sub
		c:7+.9-^3.5
		% long
		c\breve c\longa
	}

keys =  \notes{
                s1
                s1 s1 s1
                s1 s1
                s1
                \key g \major s1
                \key d \major s1 
                \key a \major s1 
                \key e \major s1 
                \key b \major s1 
                \key fis \major s1
                \key c \major s1 
                \key f \major s1 
                \key bes \major s1
                \key es \major s1
                \key as \major s1
                \key des \major s1
                \key ges \major s1
                \key c \major s1*2
                \key c \major s1*6
	}

\score{
	<
		\context ChordNames \scales
		\context Staff < \scales \keys >
	>
	\paper{
        	\translator { 
			\ChordNamesContext
			ChordName \override #'word-space = #1 
		}
	}
}
