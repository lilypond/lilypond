
\header{
filename =	 "beams.ly";
composer =	 "heu";
enteredby =	 "jcn";
copyright =	 "PD";

TestedFeatures =	 "beams and beamflags";
}
\version "0.1.7";

\score{
	\melodic { \multi 3 < 
		{ \octave c'; 
			\meter 8/4;
			\duration8;
			\stemup [c c'' a' f']
			\duration16;
			\stemup [c c'' a' f']
			\duration32;
			\stemup [c c'' a' f']
			\duration64;
			\stemup [c c'' a' f']
			\duration128;
			\stemup [c c'' a' f']
			r32

			\duration 8;
			\stemup ['g 'g 'g 'g]
			\duration 16;
			\stemup ['g 'g 'g 'g]
			\duration 32;
			\stemup ['g 'g 'g 'g]
			\duration 64;
			\stemup ['g 'g 'g 'g]
			\duration 128;
			\stemup ['g 'g 'g 'g]
			r32
		}
	
	{ \octave c'; 
			\meter 8/4;
			\duration8;
			\stemdown [a' 'a c e]
			\duration16;
			\stemdown [a' 'a c e]
			\duration32;
			\stemdown [a' 'a c e]
			\duration64;
			\stemdown [a' 'a c e]
			\duration128;
			\stemdown [a' 'a c e]
			r32

			\octave c;
			\duration 8;
			\stemdown [d'' d'' d'' d'']
			\duration 16;
			\stemdown [d'' d'' d'' d'']
			\duration 32;
			\stemdown [d'' d'' d'' d'']
			\duration 64;
			\stemdown [d'' d'' d'' d'']
			\duration 128;
			\stemdown [d'' d'' d'' d'']
			r32
		} >
		{
			% ugh, "duration mode"
			\duration 8;
			[g8 g] [a a] [b b] [c' c']
			\duration 16;
			[g16 g g g] [a a a a] [b b b b] [c' c' c' c']
			\meter 4/4;
			\duration 32;
			[g32 g g g g g g g] [a a a a a a a a] 
			[b b b b b b b b] [c' c' c' c' c' c' c' c']
			\duration 64;
			\meter 2/4;
			[g64 g g g g g g g g g g g g g g g]
			[c' c' c' c' c' c' c' c' c' c' c' c' c' c' c' c']
		}
	}
	\paper{
		gourlay_maxmeasures = 1.;
	}
}


