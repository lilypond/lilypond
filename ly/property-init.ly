% property-init.ly

\version "2.1.22"

stemUp = \override Stem  #'direction = #1
stemDown = \override Stem  #'direction = #-1 
stemBoth= \revert Stem #'direction

slurUp   = \override Slur  #'direction = #1
slurDown = \override Slur  #'direction = #-1
slurBoth = \revert Slur #'direction 

% There's also dash, but setting dash period/length should be fixed.
slurDotted = \override Slur  #'dashed = #1
slurSolid = \revert Slur #'dashed


phrasingSlurUp   = \override PhrasingSlur  #'direction = #1
phrasingSlurDown = \override PhrasingSlur  #'direction = #-1
phrasingSlurBoth = \revert PhrasingSlur #'direction 

shiftOn  = \override NoteColumn  #'horizontal-shift = #1
shiftOnn  = \override NoteColumn  #'horizontal-shift = #2
shiftOnnn  = \override NoteColumn  #'horizontal-shift = #3
shiftOff  = \revert NoteColumn #'horizontal-shift 

tieUp = \override Tie  #'direction = #1
tieDown = \override Tie  #'direction = #-1
tieBoth = \revert Tie #'direction 

tieDotted = \override Tie  #'dashed = #1
tieSolid = \revert Tie #'dashed


dynamicUp  = {
  \override DynamicText  #'direction = #1
  \override DynamicLineSpanner  #'direction = #1
}
dynamicDown = {
  \override DynamicText  #'direction = #-1
  \override DynamicLineSpanner  #'direction = #-1
}
dynamicBoth = {
  \revert DynamicText #'direction
  \revert DynamicLineSpanner #'direction
}

scriptUp  = {
  \override TextScript  #'direction = #1
  \override Script  #'direction = #1
}
scriptDown = {
  \override TextScript  #'direction = #-1
  \override Script  #'direction = #-1
}
scriptBoth = {
  \revert TextScript #'direction
  \revert Script #'direction
}

dotsUp = \override Dots  #'direction = #1
dotsDown = \override Dots  #'direction = #-1
dotsBoth = \revert Dots #'direction 

tupletUp  =   \override TupletBracket  #'direction = #1
tupletDown =   \override TupletBracket  #'direction = #-1
tupletBoth =   \revert TupletBracket #'direction

cadenzaOn = \set Timing.timing =  ##f
cadenzaOff = {
  \set Timing.timing =  ##t
  \set Timing.measurePosition =  #(ly:make-moment 0 1)
}

newpage = \notes
{
  \break
  % urg, only works for TeX output
  \context Score \applyoutput
  #(outputproperty-compatibility (make-type-checker 'paper-column-interface)
    'between-system-string "\\newpage")
}

% dynamic ly:dir?  text script, articulation script ly:dir?	
oneVoice = #(context-spec-music (make-voice-props-revert) 'Voice)
voiceOne = #(context-spec-music (make-voice-props-set 0) 'Voice)
voiceTwo = #(context-spec-music (make-voice-props-set 1) 'Voice)
voiceThree =#(context-spec-music (make-voice-props-set 2) 'Voice)
voiceFour = #(context-spec-music (make-voice-props-set 3) 'Voice)

	
tiny  = 
	\set fontSize =  #-2

small  = 
	\set fontSize =  #-1

normalsize = {
	\set fontSize =  #0
}


% End the incipit and print a ``normal line start''.
endincipit = \notes \context Staff {
    \partial 16 s16  % Hack to handle e.g. \bar ".|" \endincipit
    \once \override Staff.Clef  #'full-size-change = ##t
    \once \override Staff.Clef  #'non-default = ##t
    \bar ""
}

autoBeamOff = \set autoBeaming =  ##f
autoBeamOn = \set autoBeaming =  ##t

fatText = \override TextScript  #'no-spacing-rods = ##f
emptyText = \override TextScript  #'no-spacing-rods  = ##t

showStaffSwitch = \set followVoice =  ##t
hideStaffSwitch = \set followVoice =  ##f

% accidentals as they were common in the 18th century.
defaultAccidentals = {
  \set Current.extraNatural =  ##t
  \set Current.autoAccidentals =  #'(Staff (same-octave . 0))
  \set Current.autoCautionaries =  #'()
}

% accidentals in voices instead of staves.
% Notice that accidentals from one voice do NOT get cancelled in other voices
voiceAccidentals = {
  \set Current.extraNatural =  ##t
  \set Current.autoAccidentals =  #'(Voice (same-octave . 0))
  \set Current.autoCautionaries =  #'()
  
}

% accidentals as suggested by Kurt Stone, Music Notation in the 20th century.
% This includes all the default accidentals, but accidentals also needs cancelling
% in other octaves and in the next measure.
modernAccidentals = {
  \set Current.extraNatural =  ##f
  \set Current.autoAccidentals =  #'(Staff (same-octave . 0) (any-octave . 0) (same-octave . 1))
  \set Current.autoCautionaries =  #'()  
}

% the accidentals that Stone adds to the old standard as cautionaries
modernCautionaries = {
  \set Current.extraNatural =  ##f
  \set Current.autoAccidentals =  #'(Staff (same-octave . 0))
  \set Current.autoCautionaries =  #'(Staff (any-octave . 0) (same-octave . 1))  
}

% Multivoice accidentals to be read both by musicians playing one voice
% and musicians playing all voices.
% Accidentals are typeset for each voice, but they ARE cancelled across voices.
modernVoiceAccidentals = {
  \set Current.extraNatural =  ##f
  \set Current.autoAccidentals =  #'(
    Voice (same-octave . 0) (any-octave . 0) (same-octave . 1)
    Staff (same-octave . 0) (any-octave . 0) (same-octave . 1)
  )
  \set Current.autoCautionaries =  #'()  
}

% same as modernVoiceAccidental eccept that all special accidentals are typeset
% as cautionaries
modernVoiceCautionaries = {
  \set Current.extraNatural =  ##f
  \set Current.autoAccidentals =  #'(
    Voice (same-octave . 0) 
  )
  \set Current.autoCautionaries =  #'(
    Voice (any-octave . 0) (same-octave . 1)
    Staff (same-octave . 0) (any-octave . 0) (same-octave . 1)
  )  
}

% stone's suggestions for accidentals on grand staff.
% Accidentals are cancelled across the staves in the same grand staff as well
pianoAccidentals = {
  \set Current.autoAccidentals =  #'(
    Staff (same-octave . 0) (any-octave . 0) (same-octave . 1)
    GrandStaff (any-octave . 0) (same-octave . 1)
  )
  \set Current.autoCautionaries =  #'()  
}

pianoCautionaries = {
  \set Current.autoAccidentals =  #'(
    Staff (same-octave . 0)
  )
  \set Current.autoCautionaries =  #'(
    Staff (any-octave . 0) (same-octave . 1)
    GrandStaff (any-octave . 0) (same-octave . 1)
  )  
}


% Do not reset the key at the start of a measure.  Accidentals will be
% printed only once and are in effect until overridden, possibly many
% measures later.
noResetKey = {
  \set Current.autoAccidentals =  #'(Staff (same-octave . #t))
  \set Current.autoCautionaries =  #'()
}

% do not set localKeySignature when a note alterated differently from
% localKeySignature is found.
% Causes accidentals to be printed at every note instead of
% remembered for the duration of a measure.
% accidentals not being remembered, causing accidentals always to be typeset relative to the time signature
forgetAccidentals = {
  \set Current.autoAccidentals =  #'(Staff (same-octave . -1))
  \set Current.autoCautionaries =  #'()  
}


% To remove a Volta bracket or some other graphical object,
% set it to turnOff. Example: \set Staff.VoltaBracket =  \turnOff

%%
%% DO NOT USE THIS. IT CAN LEAD TO CRASHES.
turnOff = #(cons '() '())

% For drawing vertical chord brackets with \arpeggio
% This is a shorthand for the value of the print-function property 
% of either Staff.Arpeggio or PianoStaff.Arpeggio, depending whether 
% cross-staff brackets are desired. 

arpeggioBracket = #Arpeggio::brew_chord_bracket
arpeggio = #(make-music-by-name 'ArpeggioEvent)
glissando = #(make-music-by-name 'GlissandoEvent)

fermataMarkup = \markup { \musicglyph #"scripts-ufermata" } 

setMmRestFermata =
  \once \override MultiMeasureRestNumber  #'text =
    #fermataMarkup 


hideNotes =\sequential {
				% hide notes, accidentals, etc.
    \override Dots  #'transparent = ##t
    \override NoteHead  #'transparent = ##t
    \override Stem  #'transparent = ##t
    \override Beam  #'transparent = ##t
    \override Staff.Accidental  #'transparent = ##t
}


unHideNotes =  \sequential {
  \revert Staff.Accidental #'transparent
  \revert Beam #'transparent
  \revert Stem #'transparent
  \revert NoteHead #'transparent
  \revert Dots #'transparent 
}

germanChords = {
    \set chordRootNamer =  #(chord-name->german-markup #t)
    \set chordNoteNamer =  #note-name->german-markup
}
semiGermanChords = {
    \set chordRootNamer =  #(chord-name->german-markup #f)
    \set chordNoteNamer =  #note-name->german-markup
}
