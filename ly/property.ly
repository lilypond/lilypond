% property.ly
% list of properties that lily recognises
% and some shorthands (ugh)

%{

SEE THE REFERENCE MANUAL FOR EXPLANATIONS.

%}

\version "1.2.0";

%hmm, (these) abbrevs suck, imo
% i guess they're meant as some form of doco
% that's what i use them for...
stemup =        \property Voice.verticalDirection = \up 
stemboth= 	\property Voice.verticalDirection = \center
stemdown = 	\property Voice.verticalDirection = \down

slurup = \notes {
	s1*0
	\property Voice.slurVerticalDirection = \up 
	}
slurboth= \notes {
	s1*0
	\property Voice.slurVerticalDirection = \center
}
slurdown = \notes { 	
	s1*0
	\property Voice.slurVerticalDirection = \down
}

shifton = \property Voice.horizontalNoteShift = 1
shiftoff = \property Voice.horizontalNoteShift = 0

onevoice = { 	
	\stemboth \shiftoff	
}

%{ THESE ARE DEPRECATED  %}
voiceone = 
	\context Voice = one  {
	\stemup
}

voicetwo = 
	\context Voice = two {
	\stemdown
}

voicethree = 
	\context Voice = three {
	\stemup

}

voicefour = 
	\context Voice = four {
	\stemdown
	\shifton
}

%{ END OF DEPRECATED %}


% ugh, cluttering global namespace...

% ugh2. 
none=0
free=0
normal=1
traditional=2
infinity=10000

beamslopeproportional = 
	\property Score.beamslopedamping = \none

beamslopedamped = 
	\property Score.beamslopedamping = \normal


beamslopezero = 
	\property Score.beamslopedamping = \infinity


% this sucks, you'd want to pass an array, at least
% (or embedded code: you still can't dictate the slope / stemlength)
beamposfree = 
	\property Score.beamquantisation = \none


beamposnormal = 
	\property Score.beamquantisation = \normal


beampostraditional = 
	\property Score.beamquantisation = \traditional


slurnormal = 
	\property Voice.slurDash = ""


slurdotted = 
	\property Voice.slurDash = 1


tupletoff = {
	\property Voice.tupletVisibility = 0
}
tupleton = {
	\property Voice.tupletVisibility = 3
}
tiny  = {
	\property Voice.fontSize= "-2"
}

small  = {
	\property Voice.fontSize= "-1"
}

normalsize = {
	\property Voice.fontSize= "0"
}

normalkey = {
	\property Staff.keyOctaviation = 1
}

specialkey = {
	\property Staff.keyOctaviation = 0
}

% End the incipit and print a ``normal line start''.
endincipit = \notes{
    \partial 16; s16  % Hack to handle e.g. \bar ".|"; \endincipit
    \property Staff.clefStyle = "fullSizeChanges" 
    \nobreak \bar "";
}

autoBeamOff = \property Voice.noAutoBeaming = "1"
autoBeamOn = \property Voice.noAutoBeaming = ""  
