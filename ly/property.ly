% property.ly

\version "1.3.110";

stemUp = \property Voice.Stem \override #'direction = #1
stemDown = \property Voice.Stem \override #'direction = #-1 
stemBoth= \property Voice.Stem \revert #'direction

slurUp   = \property Voice.Slur \override #'direction = #1
slurBoth = \property Voice.Slur \revert #'direction 
slurDown = \property Voice.Slur \override #'direction = #-1
shiftOn  = \property Voice.NoteColumn \override #'horizontal-shift = #1
shiftOnn  = \property Voice.NoteColumn \override #'horizontal-shift = #2
shiftOnnn  = \property Voice.NoteColumn \override #'horizontal-shift = #3
shiftOff  = \property Voice.NoteColumn \revert #'horizontal-shift 


tieUp = \property Voice.Tie \override #'direction = #1
tieDown = \property Voice.Tie \override #'direction = #-1
tieBoth = \property Voice.Tie \revert #'direction 

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

slurDotted = \property Voice.Slur \override #'dashed = #1
slurNoDots = \property Voice.Slur \revert #'dashed

	
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

