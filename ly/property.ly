% property.ly

\version "1.3.96";

stemUp = \property Voice.Stem \push #'direction = #1
stemDown = \property Voice.Stem \push #'direction = #-1 
stemBoth= \property Voice.Stem \pop #'direction

slurUp   = \property Voice.Slur \push #'direction = #1
slurBoth = \property Voice.Slur \pop #'direction 
slurDown = \property Voice.Slur \push #'direction = #-1
shiftOn  = \property Voice.NoteColumn \push #'horizontal-shift = #1
shiftOnn  = \property Voice.NoteColumn \push #'horizontal-shift = #2
shiftOnnn  = \property Voice.NoteColumn \push #'horizontal-shift = #3
shiftOff  = \property Voice.NoteColumn \pop #'horizontal-shift 


tieUp = \property Voice.Tie \push #'direction = #1
tieDown = \property Voice.Tie \push #'direction = #-1
tieBoth = \property Voice.Tie \pop #'direction 

cadenzaOn = \property Score.timing = ##f
cadenzaOff = { \property Score.timing = ##t
	\property Score.measurePosition = #(make-moment 0 1)
	}

	
oneVoice = { 	
	\stemBoth
	\tieBoth
	\shiftOff
}

voiceOne = { \stemUp
   \tieUp
}
voiceTwo = { \stemDown
   \tieDown
   }
   
voiceThree = {
	\stemUp
	\shiftOn
}

voiceFour = {
	\stemDown
	\shiftOn
}

slurDotted = \property Voice.Slur \push #'dash = #1
slurNoDots = \property Voice.Slur \pop #'dash

	
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

