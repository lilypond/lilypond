%{MudelaHeader

 filename: beams.ly
 title: 
 description: 
 composer(s): heu
 entered-by: jcn
 copyright: GPL

 Tested Features: beams and beamflags
EndMudelaHeader
%}

\score{
	\staff{ \inputregister{melodicregs}
		\melodic{ \octave c'; 
			\meter 8/4;
			\duration8;
			\stem 1; [c c'' a' f']
			\duration16;
			\stem 1; [c c'' a' f']
			\duration32;
			\stem 1; [c c'' a' f']
			\duration64;
			\stem 1; [c c'' a' f']
			\duration128;
			\stem 1; [c c'' a' f']

			\duration 8;
			\stem 1; ['g 'g 'g 'g]
			\duration 16;
			\stem 1; ['g 'g 'g 'g]
			\duration 32;
			\stem 1; ['g 'g 'g 'g]
			\duration 64;
			\stem 1; ['g 'g 'g 'g]
			\duration 128;
			\stem 1; ['g 'g 'g 'g]
		}
	}
	\staff{ \inputregister{melodicregs}
		\melodic{ \octave c'; 
			\meter 8/4;
			\duration8;
			\stem -1; [a' 'a c e]
			\duration16;
			\stem -1; [a' 'a c e]
			\duration32;
			\stem -1; [a' 'a c e]
			\duration64;
			\stem -1; [a' 'a c e]
			\duration128;
			\stem -1; [a' 'a c e]

			\octave c;
			\duration 8;
			\stem -1; [d'' d'' d'' d'']
			\duration 16;
			\stem -1; [d'' d'' d'' d'']
			\duration 32;
			\stem -1; [d'' d'' d'' d'']
			\duration 64;
			\stem -1; [d'' d'' d'' d'']
			\duration 128;
			\stem -1; [d'' d'' d'' d'']
		}
	}
	\paper{}
}
