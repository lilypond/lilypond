%{MudelaHeader

 filename: beams.ly
 title: 
 description: 
 composer(s): heu
 entered-by: jcn
 copyright: PD

 Tested Features: beams and beamflags
EndMudelaHeader
%}

\score{
	\melodic <\multi 3; 
		{ \octave c'; 
			\meter 8/4;
			\duration8;
			\stem \up; [c c'' a' f']
			\duration16;
			\stem \up; [c c'' a' f']
			\duration32;
			\stem \up; [c c'' a' f']
			\duration64;
			\stem \up; [c c'' a' f']
			\duration128;
			\stem \up; [c c'' a' f']

			\duration 8;
			\stem \up; ['g 'g 'g 'g]
			\duration 16;
			\stem \up; ['g 'g 'g 'g]
			\duration 32;
			\stem \up; ['g 'g 'g 'g]
			\duration 64;
			\stem \up; ['g 'g 'g 'g]
			\duration 128;
			\stem \up; ['g 'g 'g 'g]
		}
	
	{ \octave c'; 
			\meter 8/4;
			\duration8;
			\stem \down; [a' 'a c e]
			\duration16;
			\stem \down; [a' 'a c e]
			\duration32;
			\stem \down; [a' 'a c e]
			\duration64;
			\stem \down; [a' 'a c e]
			\duration128;
			\stem \down; [a' 'a c e]

			\octave c;
			\duration 8;
			\stem \down; [d'' d'' d'' d'']
			\duration 16;
			\stem \down; [d'' d'' d'' d'']
			\duration 32;
			\stem \down; [d'' d'' d'' d'']
			\duration 64;
			\stem \down; [d'' d'' d'' d'']
			\duration 128;
			\stem \down; [d'' d'' d'' d'']
		}
	>}


