\header{
filename =	 "beams.ly";
composer =	 "heu";
enteredby =	 "jcn";
copyright =	 "PD";

TestedFeatures =	 "beams and beamflags";
}
\version "0.1.15";

\score{
	<
%		\property Score.beamquantisation = \none
		\type Grand_staff < 
		\melodic { 
			\octave c'; 
			\meter 8/4;
			\stemup [c8 c'' a' f']
			\stemup [c16 c'' a' f']
			\stemup [c32 c'' a' f']
			\stemup [c64 c'' a' f']
			\stemup [c128 c'' a' f']
			r32

			\stemup [g8 g g g]
			\stemup [g16 g g g]
			\stemup [g32 g g g]
			\stemup [g64 g g g]
			\stemup [g128 g g g]
			r32

			\octave c;
			\stemboth;
			[c'8 c'] [b b] [a a] [g g] [f f] [e e]
			[c'16 c'] [b b] [a a] [g g]  [f f] [e e]
			[c'32 c'] [b b] [a a] [g g] [f f] [e e]
			[c'64 c'] [e e]
		}
	>
	<	
		\melodic { 
			\octave c';  
			\meter 8/4;
			\stemdown [a'8 a, c e]
			\stemdown [a'16 a, c e]
			\stemdown [a'32 a, c e]
			\stemdown [a'64 a, c e]
			\stemdown [a'128 a, c e]
			r32

			\octave c;
			\stemdown [d''8 d'' d'' d'']
			\stemdown [d''16 d'' d'' d'']
			\stemdown [d''32 d'' d'' d'']
			\stemdown [d''64 d'' d'' d'']
			\stemdown [d''128 d'' d'' d'']
			r32

			\octave c'';
			\stemboth;
			[a8 a] [b b] [c' c'] [d' d'] [e' e'] [f' f']
			[a16 a] [b b] [c' c'] [d' d']  [e' e'] [f' f']
			[a32 a] [b b] [c' c'] [d' d']  [e' e'] [f' f']
			[a64 a] [f' f'] 
		}
	>
	>

	\paper{
		castingalgorithm = \Wordwrap;
	}
}

