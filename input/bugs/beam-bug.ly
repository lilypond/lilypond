% silly file to test beam stem lengths
% compare to *ugly* bug in preludes-1: [e'16 c'' g' f']
% here it's okee ??

one = \melodic{
	\octave c';
	[e'8 c'' g' f']
	\octave c'';
  [e16-1\f c'-5( g-3 f-2] % [e-1 c'-5 g-3 e-2] 
	\octave c';
%	      [e'16 c'' g' f']
	[e'32 c'' g' f']
	[c32 c] ['b b,]
	[c8 c]
	[e8 g b]
	[e16 g b] c c c
	[e32 g b] c
	[c c c c]
	c4 c4
	\onevoice;
	[e'8 c'' g' f']
	[e'16 c'' g' f']
	[e'32 c'' g' f']
	[c32 c] ['b b,]
	[c8 c]
	[e8 g b]
	[e16 g b] c
	[e32 g b] c
	[c c c c]
	c4
}

\score{
	\one
}
