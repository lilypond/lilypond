\header{
filename =	 "multi.ly";
description =	 "stupid testfile for pl68 features.";
enteredby =	 "HWN";
copyright =	 "public domain";
}
%{
Tested Features: \multi


WARNING: the \multi construct is not very intuitive.  You better try
the

	\type Grand_staff <
		\type Staff = treble {.. }
		\type Staff = bass {.. }
	>		

construct to get multiple staffs, it's cleaner

%}


\version "0.1.15";

\score{
	\melodic 
		{ \octave c'; c4 c4 
			\multi 1 <  { c2 c2 } { c'2 c'2 } > 
			\multi 2 <  { \stemdown c2 c2 } { \stemup c'2 c'2 } > 
			\multi 3 < { \clef "bass"; c2 c2 } 
                           { \meter 2/4;\bar "||";
				 \key fis cis gis; c'2 c'2 } 
> 
	 			c2 c1 
			c1 c1
			\multi 1 < \multi 3 < 
				{ \meter 2/4; \clef "violin"; c2 c2 }
				{ \meter 2/4; \clef "bass"; c2 c2 }
			>
			\multi 3 < 
				{ \meter 2/4; \clef "violin"; c2 c2 }
				{ \meter 2/4; \clef "bass"; c2 c2 }
			>
			>
		}
}
