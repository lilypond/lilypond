% property.ly

\version "1.3.59";

stemup = \property Voice.basicStemProperties \push #'direction = #1
stemdown = \property Voice.basicStemProperties \push #'direction = #-1 
stemboth= \property basicStemProperties \pop #'direction

slurup   = \property Voice.basicSlurProperties \push #'direction = #1
slurboth = \property basicSlurProperties \pop #'direction 
slurdown = \property Voice.basicSlurProperties \push #'direction = #-1
shifton  = \property Voice.basicNoteColumnProperties \push #'horizontal-shift = #1
shiftonn  = \property Voice.basicNoteColumnProperties \push #'horizontal-shift = #2
shiftonnn  = \property Voice.basicNoteColumnProperties \push #'horizontal-shift = #3
shiftoff  = \property basicNoteColumnProperties \pop #'horizontal-shift 


tieUp = \property Voice.basicTieProperties \push #'direction = #1
tieDown = \property Voice.basicTieProperties \push #'direction = #-1
tieBoth = \property basicTieProperties \pop #'direction 

cadenzaOn = \property Score.timing = ##f
cadenzaOff = { \property Score.timing = ##t
	\property Score.measurePosition = #(make-moment 0 1)
	}

	
onevoice = { 	
	\stemboth
	\tieBoth
}

voiceone = { \stemup
   \tieUp
}
voicetwo = { \stemdown
   \tieDown
   }
   
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
%{

slurnormal = 
	\property Voice.slurDash = ##f


slurdotted =				
	\property Voice.slurDash = 1


tupletoff =
	\property Voice.tupletVisibility = 0

tupleton = 
	\property Voice.tupletVisibility = 3
%}

	
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

