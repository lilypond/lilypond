\header{
filename =	 "beam-pos.ly";
composer =	 "jcn";
enteredby =	 "jcn";
copyright =	 "PD";

TestedFeatures =	 "beam quant positions";
}

\score{
	\notes\relative c' { 
		[c8 c] [a'' a]
		[a, a] [c c]
		[d,8 d] [g' g]
		[g, g] [d' d]
		[c,16 c c c] [a'' a a a]
		[a, a a a] [c c c c]
		\break
		[c,32 c c c c c c c] [a'' a a a a a a a]
		[f, f f f f f f f] [e' e e e e e e e]
		\break
		[c,8 d] [a'' g]
		[g, f] [d' e]
		\break
	}
	\paper{
	}
}

