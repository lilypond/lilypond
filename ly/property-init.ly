% property-init.ly

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

dotsUp = \property Voice.Dots \override #'direction = #1
dotsDown = \property Voice.Dots \override #'direction = #-1
dotsBoth = \property Voice.Dots \revert #'direction 

% why doubly?
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
oneVoice = #(context-spec-music (make-voice-props-revert) "Voice")
voiceOne = #(context-spec-music (make-voice-props-set 0) "Voice")
voiceTwo = #(context-spec-music (make-voice-props-set 1) "Voice")
voiceThree =#(context-spec-music (make-voice-props-set 2) "Voice")
voiceFour = #(context-spec-music (make-voice-props-set 3) "Voice")

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

showStaffSwitch = \property Voice.followVoice = ##t
hideStaffSwitch = \property Voice.followVoice = ##f

% FIXME: Move this docu (to where?)

% accidentals as they were common in the 18th century.
defaultAccidentals = {
  \property Score.extraNatural = ##t
  \property Score.autoAccidentals = #'(Staff (same-octave . 0))
  \property Score.autoCautionaries = #'()
}

% accidentals in voices instead of staves.
% Notice that accidentals from one voice do NOT get cancelled in other voices
voiceAccidentals = {
  \property Score.extraNatural = ##t
  \property Score.autoAccidentals = #'(Voice (same-octave . 0))
  \property Score.autoCautionaries = #'()
  
}

% accidentals as suggested by Kurt Stone, Music Notation in the 20th century.
% This includes all the default accidentals, but accidentals also needs cancelling
% in other octaves and in the next measure.
modernAccidentals = {
  \property Score.extraNatural = ##f
  \property Score.autoAccidentals = #'(Staff (same-octave . 0) (any-octave . 0) (any-octave . 1))
  \property Score.autoCautionaries = #'()  
}

% the accidentals that Stone adds to the old standard as cautionaries
modernCautionaries = {
  \property Score.extraNatural = ##f
  \property Score.autoAccidentals = #'(Staff (same-octave . 0))
  \property Score.autoCautionaries = #'(Staff (any-octave . 0) (any-octave . 1))  
}

% Multivoice accidentals to be read both by musicians playing one voice
% and musicians playing all voices.
% Accidentals are typeset for each voice, but they ARE cancelled across voices.
modernVoiceAccidentals = {
  \property Staff.extraNatural = ##f
  \property Staff.autoAccidentals = #'(
    Voice (same-octave . 0) (any-octave . 0) (any-octave . 1)
    Staff (same-octave . 0) (any-octave . 0) (any-octave . 1)
  )
  \property Staff.autoCautionaries = #'()  
}

% same as modernVoiceAccidental eccept that all special accidentals are typeset
% as cautionaries
modernVoiceCautionaries = {
  \property Staff.extraNatural = ##f
  \property Staff.autoAccidentals = #'(
    Voice (same-octave . 0) 
  )
  \property Staff.autoCautionaries = #'(
    Voice (any-octave . 0) (any-octave . 1)
    Staff (same-octave . 0) (any-octave . 0) (any-octave . 1)
  )  
}

% stone's suggestions for accidentals on grand staff.
% Accidentals are cancelled across the staves in the same grand staff as well
pianoAccidentals = {
  \property GrandStaff.autoAccidentals = #'(
    Staff (same-octave . 0) (any-octave . 0) (any-octave . 1)
    GrandStaff (any-octave . 1)
  )
  \property GrandStaff.autoCautionaries = #'()  
}

% Do not reset the key at the start of a measure.  Accidentals will be
% printed only once and are in effect until overridden, possibly many
% measures later.
noResetKey = {
  \property Score.autoAccidentals = #'(Staff (same-octave . #t))
  \property Score.autoCautionaries = #'()
}

% do not set localKeySignature when a note alterated differently from
% localKeySignature is found.
% Causes accidentals to be printed at every note instead of
% remembered for the duration of a measure.
% accidentals not being remembered, causing accidentals always to be typeset relative to the time signature
forgetAccidentals = {
  \property Score.autoAccidentals = #'(Staff (same-octave . -1))
  \property Score.autoCautionaries = #'()  
}


% To remove a Volta bracket or some other graphical object,
% set it to turnOff. Example: \property Staff.VoltaBracket = \turnOff

turnOff = #'()
