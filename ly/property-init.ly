% property-init.ly

\version "2.4.0"

stemUp = \override Stem  #'direction = #1
stemDown = \override Stem  #'direction = #-1 
stemNeutral= \revert Stem #'direction

slurUp = \override Slur  #'direction = #1
slurDown = \override Slur  #'direction = #-1
slurNeutral = \revert Slur #'direction 

% There's also dash, but setting dash period/length should be fixed.
slurDashed = {
	\override Slur #'dash-period = #1
	\override Slur #'dash-fraction = #0.4
}
slurDotted = {
	\override Slur  #'dash-period = #1
	\override Slur #'dash-fraction = #0.1
}
slurSolid = {
	\revert Slur #'dash-period
	\revert Slur #'dash-fraction
}


phrasingSlurUp = \override PhrasingSlur  #'direction = #1
phrasingSlurDown = \override PhrasingSlur  #'direction = #-1
phrasingSlurNeutral = \revert PhrasingSlur #'direction 

shiftOn = \override NoteColumn  #'horizontal-shift = #1
shiftOnn = \override NoteColumn  #'horizontal-shift = #2
shiftOnnn = \override NoteColumn  #'horizontal-shift = #3
shiftOff = \revert NoteColumn #'horizontal-shift 

tieUp = \override Tie  #'direction = #1
tieDown = \override Tie  #'direction = #-1
tieNeutral = \revert Tie #'direction 

tieDotted = \override Tie  #'dashed = #1
tieSolid = \revert Tie #'dashed

setEasyHeads = \sequential {
	\override NoteHead #'print-function = #Note_head::brew_ez_stencil
	\override NoteHead #'Y-extent-callback = #'()
	\override NoteHead #'X-extent-callback = #'()
}

dynamicUp = {
  \override DynamicText  #'direction = #1
  \override DynamicLineSpanner  #'direction = #1
}
dynamicDown = {
  \override DynamicText  #'direction = #-1
  \override DynamicLineSpanner  #'direction = #-1
}
dynamicNeutral = {
  \revert DynamicText #'direction
  \revert DynamicLineSpanner #'direction
}


dotsUp = \override Dots  #'direction = #1
dotsDown = \override Dots  #'direction = #-1
dotsNeutral = \revert Dots #'direction 

tupletUp = \override TupletBracket  #'direction = #1
tupletDown = \override TupletBracket  #'direction = #-1
tupletNeutral = \revert TupletBracket #'direction

cadenzaOn = \set Timing.timing = ##f
cadenzaOff = {
  \set Timing.timing = ##t
  \set Timing.measurePosition = #(ly:make-moment 0 1)
}

% dynamic ly:dir?  text script, articulation script ly:dir?	
oneVoice = #(context-spec-music (make-voice-props-revert) 'Voice)
voiceOne = #(context-spec-music (make-voice-props-set 0) 'Voice)
voiceTwo = #(context-spec-music (make-voice-props-set 1) 'Voice)
voiceThree =#(context-spec-music (make-voice-props-set 2) 'Voice)
voiceFour = #(context-spec-music (make-voice-props-set 3) 'Voice)

	
tiny = 
	\set fontSize = #-2

small = 
	\set fontSize = #-1

normalsize = {
	\set fontSize = #0
}


% End the incipit and print a ``normal line start''.
endincipit =  \context Staff {
    \partial 16 s16  % Hack to handle e.g. \bar ".|" \endincipit
    \once \override Staff.Clef  #'full-size-change = ##t
    \once \override Staff.Clef  #'non-default = ##t
    \bar ""
}

autoBeamOff = \set autoBeaming = ##f
autoBeamOn = \set autoBeaming = ##t

fatText = \override TextScript  #'no-spacing-rods = ##f
emptyText = \override TextScript  #'no-spacing-rods = ##t

showStaffSwitch = \set followVoice = ##t
hideStaffSwitch = \set followVoice = ##f



% For drawing vertical chord brackets with \arpeggio
% This is a shorthand for the value of the print-function property 
% of either Staff.Arpeggio or PianoStaff.Arpeggio, depending whether 
% cross-staff brackets are desired. 

arpeggio = #(make-music 'ArpeggioEvent)

arpeggioUp = \sequential {
  \revert Arpeggio  #'print-function
  \override Arpeggio  #'arpeggio-direction = #1
}
arpeggioDown = \sequential {
  \revert Arpeggio  #'print-function
  \override Arpeggio  #'arpeggio-direction = #-1
}
arpeggioNeutral = \sequential {
  \revert Arpeggio  #'print-function
  \revert Arpeggio  #'arpeggio-direction
}
arpeggioBracket = \sequential {
  \override Arpeggio  #'print-function = #Arpeggio::brew_chord_bracket
}

glissando = #(make-music 'GlissandoEvent)

fermataMarkup = \markup { \musicglyph #"scripts-ufermata" } 

hideNotes =\sequential {
  % hide notes, accidentals, etc.
  \override Dots  #'transparent = ##t
  \override NoteHead  #'transparent = ##t
  \override NoteHead  #'no-ledgers = ##t
  \override Stem  #'transparent = ##t
  \override Beam  #'transparent = ##t
  \override Accidental  #'transparent = ##t
}


unHideNotes = \sequential {
  \revert Accidental #'transparent
  \revert Beam #'transparent
  \revert Stem #'transparent
  \revert NoteHead #'transparent
  \revert NoteHead #'no-ledgers
  \revert Dots #'transparent 
}

germanChords = {
    \set chordRootNamer = #(chord-name->german-markup #t)
    \set chordNoteNamer = #note-name->german-markup
}
semiGermanChords = {
    \set chordRootNamer = #(chord-name->german-markup #f)
    \set chordNoteNamer = #note-name->german-markup
}



improvisationOn =  {
    \set squashedPosition = #0
    \override NoteHead  #'style = #'slash
}

improvisationOff =  {
    \unset squashedPosition 
    \revert NoteHead #'style
}
