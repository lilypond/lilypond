% property.ly
% list of properties that lily recognises
% and some shorthands (ugh)

%{

SEE THE REFERENCE MANUAL FOR EXPLANATIONS.

%}

\version "1.3.42";

%hmm, (these) abbrevs suck, imo
% i guess they're meant as some form of doco
% that's what i use them for...

stemup =        \property Voice.verticalDirection = \up 
stemboth= 	\property Voice.verticalDirection = \center
stemdown = 	\property Voice.verticalDirection = \down

slurup   = \property Voice.slurVerticalDirection = \up 
slurboth = \property Voice.slurVerticalDirection = \center
slurdown = \property Voice.slurVerticalDirection = \down
shifton  = \property Voice.horizontalNoteShift = #1
shiftoff = \property Voice.horizontalNoteShift = #0

cadenzaOn = \property Score.timing = ##f
cadenzaOff = { \property Score.timing = ##t
	\property Score.measurePosition = #(make-moment 0 1)
	}

	
onevoice = { 	
	\stemboth \shiftoff	
}

voiceone = \stemup
voicetwo = \stemdown
voicethree = {
	\stemup
	\shifton
}

voicefour = {
	\stemdown
	\shifton
}

% ugh, cluttering global namespace...

% ugh2. 
infinity=10000

beamslopeproportional = 
	\property Score.beamSlopedamping = 0

beamslopedamped = 
	\property Score.beamSlopedamping = 1


beamslopezero = 
	\property Score.beamSlopedamping = \infinity


% this sucks, you'd want to pass an array, at least
% (or embedded code: you still can't dictate the slope / stemlength)
beamposfree = 
	\property Score.beamQuantisation = 0


beamposnormal = 
	\property Score.beamQuantisation = 1


beampostraditional = 
	\property Score.beamQuantisation = 2


slurnormal = 
	\property Voice.slurDash = ##f


slurdotted =				
	\property Voice.slurDash = 1


tupletoff =
	\property Voice.tupletVisibility = 0

tupleton = 
	\property Voice.tupletVisibility = 3

tiny  = 
	\property Voice.fontSize= -2


small  = 
	\property Voice.fontSize= -1


normalsize = {
	\property Voice.fontSize= 0
}

normalkey = {
	\property Staff.keyOctaviation = ##f
}

specialkey = {
	\property Staff.keyOctaviation = ##t
}

% End the incipit and print a ``normal line start''.
endincipit = \notes{
    \partial 16; s16  % Hack to handle e.g. \bar ".|"; \endincipit
    \property Staff.clefStyle = #"fullSizeChanges" 
    \bar "";
}

autoBeamOff = \property Voice.noAutoBeaming = ##t
autoBeamOn = \property Voice.noAutoBeaming = ##f


emptyText = \property Voice.textNonEmpty = ##f
fatText = \property Voice.textNonEmpty = ##t

