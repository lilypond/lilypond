% property.ly

\version "1.3.146"

stemUp = \property Voice.Stem \set #'direction = #1
stemDown = \property Voice.Stem \set #'direction = #-1 
stemBoth= \property Voice.Stem \revert #'direction

slurUp   = \property Voice.Slur \override #'direction = #1
slurDown = \property Voice.Slur \override #'direction = #-1
slurBoth = \property Voice.Slur \revert #'direction 
shiftOn  = \property Voice.NoteColumn \override #'horizontal-shift = #1
shiftOnn  = \property Voice.NoteColumn \override #'horizontal-shift = #2
shiftOnnn  = \property Voice.NoteColumn \override #'horizontal-shift = #3
shiftOff  = \property Voice.NoteColumn \revert #'horizontal-shift 

tieUp = \property Voice.Tie \override #'direction = #1
tieDown = \property Voice.Tie \override #'direction = #-1
tieBoth = \property Voice.Tie \revert #'direction 

dynamicUp  = {
  \property Voice.DynamicText \override #'direction = #1
  \property Voice.DynamicLineSpanner \override #'direction = #1
}
dynamicDown = {
  \property Voice.DynamicText \override #'direction = #-1
  \property Voice.DynamicLineSpanner \override #'direction = #-1
}
dynamicBoth = {
  \property Voice.DynamicText \revert #'direction
  \property Voice.DynamicLineSpanner \revert #'direction
}

scriptUp  = {
  \property Voice.TextScript \override #'direction = #1
  \property Voice.Script \override #'direction = #1
}
scriptDown = {
  \property Voice.TextScript \override #'direction = #-1
  \property Voice.Script \override #'direction = #-1
}
scriptBoth = {
  \property Voice.TextScript \revert #'direction
  \property Voice.Script \revert #'direction
}

tupletUp  = {
  \property Voice.TupletBracket \override #'direction = #1
  \property Voice.TupletBracket \override #'direction = #1
}
tupletDown = {
  \property Voice.TupletBracket \override #'direction = #-1
  \property Voice.TupletBracket \override #'direction = #-1
}
tupletBoth = {
  \property Voice.TupletBracket \revert #'direction
  \property Voice.TupletBracket \revert #'direction
}



cadenzaOn = \property Score.timing = ##f
cadenzaOff = {
  \property Score.timing = ##t
  \property Score.measurePosition = #(make-moment 0 1)
}

newpage = {
  \break
  % urg, only works for TeX output
  \context Score \outputproperty #(make-type-checker 'paper-column-interface)
    #'between-system-string = #"\\newpage"
}

% dynamic dir?  text script, articulation script dir?	
oneVoice = { 	
  \stemBoth
  \slurBoth
  \tieBoth
  \shiftOff
}

voiceOne = {
  \stemUp
  \slurUp
  \tieUp
}

voiceTwo = {
  \stemDown
  \slurDown
  \tieDown
}
   
voiceThree = {
  \stemUp
  \slurUp
  \tieUp
  \shiftOn
}

voiceFour = {
  \stemDown
  \slurDown
  \tieDown
  \shiftOn
}

% There's also dash, but setting dash period/length should be fixed.
slurDotted = \property Voice.Slur \override #'dashed = #1
slurSolid = \property Voice.Slur \revert #'dashed
tieDotted = \property Voice.Tie \override #'dashed = #1
tieSolid = \property Voice.Tie \revert #'dashed

	
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
    \partial 16 s16  % Hack to handle e.g. \bar ".|" \endincipit
    \context Staff \outputproperty #(make-type-checker 'clef-interface) #'full-size-change = ##t
    \context Staff \outputproperty #(make-type-checker 'clef-interface) #'non-default = ##t
    \bar ""
}

autoBeamOff = \property Voice.noAutoBeaming = ##t
autoBeamOn = \property Voice.noAutoBeaming = ##f

emptyText = \property Voice.textNonEmpty = ##f
fatText = \property Voice.textNonEmpty = ##t

showStaffSwitch = \property PianoStaff.followVoice = ##t
hideStaffSwitch = \property PianoStaff.followVoice = ##f


% To remove a Volta bracet or some other graphical object,
% set it to turnOff. Example: \property Staff.VoltaBracket = \turnOff

turnOff = #'((meta .  ((interfaces . ()))))
