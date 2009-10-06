% property-init.ly

\version "2.12.0"

%% for dashed slurs, phrasing slurs, and ties
#(define (make-simple-dash-definition dash-fraction dash-period)
    (list (list 0 1 dash-fraction dash-period)))


%% arpeggios

% For drawing vertical chord brackets with \arpeggio
% This is a shorthand for the value of the print-function property
% of either Staff.Arpeggio or PianoStaff.Arpeggio, depending whether
% cross-staff brackets are desired.

arpeggio = #(make-music 'ArpeggioEvent)
arpeggioArrowUp = {
  \revert Arpeggio  #'stencil
  \revert Arpeggio #'X-extent
  \override Arpeggio  #'arpeggio-direction = #UP
}
arpeggioArrowDown = {
  \revert Arpeggio #'stencil
  \revert Arpeggio #'X-extent
  \override Arpeggio  #'arpeggio-direction = #DOWN
}
arpeggioNormal = {
  \revert Arpeggio #'stencil
  \revert Arpeggio #'X-extent
  \revert Arpeggio #'arpeggio-direction
  \revert Arpeggio #'dash-definition
}
arpeggioBracket = {
  \revert Arpeggio #'X-extent
  \override Arpeggio #'stencil = #ly:arpeggio::brew-chord-bracket
}
arpeggioParenthesis = {
  \override Arpeggio #'stencil = #ly:arpeggio::brew-chord-slur
  \override Arpeggio #'X-extent = #ly:grob::stencil-width
  \revert Arpeggio #'dash-definition
}
arpeggioParenthesisDashed = {
  \override Arpeggio #'stencil = #ly:arpeggio::brew-chord-slur
  \override Arpeggio #'X-extent = #ly:grob::stencil-width
  \override Arpeggio #'dash-definition = #'((0 1 0.4 0.75))
}


%% auto beaming

autoBeamOn  = \set autoBeaming = ##t
autoBeamOff = \set autoBeaming = ##f


%% balloon length

balloonLengthOn = {
  \override BalloonTextItem #'extra-spacing-width = #'(0 . 0)
  \override BalloonTextItem #'extra-spacing-height = #'(-inf.0 . +inf.0)
}
balloonLengthOff = {
  \override BalloonTextItem #'extra-spacing-width = #'(+inf.0 . -inf.0)
  \override BalloonTextItem #'extra-spacing-height = #'(0 . 0)
}


%% bass figures

bassFigureExtendersOn = {
  \set useBassFigureExtenders = ##t
  \set Staff.useBassFigureExtenders = ##t
}
bassFigureExtendersOff = {
  \set useBassFigureExtenders = ##f
  \set Staff.useBassFigureExtenders = ##f
}
bassFigureStaffAlignmentDown =
  \override Staff.BassFigureAlignmentPositioning #'direction = #DOWN
bassFigureStaffAlignmentUp =
  \override Staff.BassFigureAlignmentPositioning #'direction = #UP
bassFigureStaffAlignmentNeutral =
  \revert Staff.BassFigureAlignmentPositioning #'direction


%% cadenzas

cadenzaOn  = \set Timing.timing = ##f
cadenzaOff = {
  \set Timing.timing = ##t
  \set Timing.measurePosition = #ZERO-MOMENT
}


%% chord names

frenchChords = {
  \set chordRootNamer = #(chord-name->italian-markup #t)
  \set chordPrefixSpacer = #0.4
}
germanChords = {
  \set chordRootNamer = #(chord-name->german-markup #t)
  \set chordNoteNamer = #note-name->german-markup
}
semiGermanChords = {
  \set chordRootNamer = #(chord-name->german-markup #f)
  \set chordNoteNamer = #note-name->german-markup
}
italianChords = {
  \set chordRootNamer = #(chord-name->italian-markup #f)
  \set chordPrefixSpacer = #0.4
}


%% compressFullBarRests

compressFullBarRests = \set Score.skipBars = ##t
expandFullBarRests   = \set Score.skipBars = ##f


%% dots

dotsUp      = \override Dots #'direction = #UP
dotsDown    = \override Dots #'direction = #DOWN
dotsNeutral = \revert Dots #'direction


%% dynamics

dynamicUp = {
  \override DynamicText #'direction = #UP
  \override DynamicLineSpanner #'direction = #UP
}
dynamicDown = {
  \override DynamicText #'direction = #DOWN
  \override DynamicLineSpanner #'direction = #DOWN
}
dynamicNeutral = {
  \revert DynamicText #'direction
  \revert DynamicLineSpanner #'direction
}


%% easy heads

easyHeadsOn = {
  \override NoteHead #'stencil = #note-head::brew-ez-stencil
  \override NoteHead #'font-size = #-8
  \override NoteHead #'font-family = #'sans
  \override NoteHead #'font-series = #'bold
}
easyHeadsOff = {
  \revert NoteHead #'stencil
  \revert NoteHead #'font-size
  \revert NoteHead #'font-family
  \revert NoteHead #'font-series
}


%% endincipit

%% End the incipit and print a ``normal line start''.
endincipit = \context Staff {
  \partial 16 s16  % Hack to handle e.g. \bar ".|" \endincipit
  \once \override Staff.Clef #'full-size-change = ##t
  \once \override Staff.Clef #'non-default = ##t
  \bar ""
}


%% fermata markup

fermataMarkup =
#(make-music 'MultiMeasureTextEvent
   'tweaks (list
             ; Set the 'text based on the 'direction
             (cons 'text (lambda (grob)
               (if (eq? (ly:grob-property grob 'direction) DOWN)
	         (markup #:musicglyph "scripts.dfermata")
	         (markup #:musicglyph "scripts.ufermata"))))
             (cons 'outside-staff-priority 40)))


%% font sizes

teeny      = \set fontSize = #-3
tiny       = \set fontSize = #-2
small      = \set fontSize = #-1
normalsize = \set fontSize = #0
large      = \set fontSize = #1
huge       = \set fontSize = #2


%% glissando

glissando = #(make-music 'GlissandoEvent)


%% harmonics

harmonicsOn  = \override NoteHead #'style = #'harmonic
harmonicsOff = \revert NoteHead #'style


%% hideNotes

hideNotes = {
  % hide notes, accidentals, etc.
  \override Dots #'transparent = ##t
  \override NoteHead #'transparent = ##t
  \override NoteHead #'no-ledgers = ##t
  \override Stem #'transparent = ##t
  \override Beam #'transparent = ##t
  \override Accidental #'transparent = ##t
}
unHideNotes = {
  \revert Accidental #'transparent
  \revert Beam #'transparent
  \revert Stem #'transparent
  \revert NoteHead #'transparent
  \revert NoteHead #'no-ledgers
  \revert Dots #'transparent
}


%% improvisation

improvisationOn = {
  \set squashedPosition = #0
  \override NoteHead #'style = #'slash
  \override Accidental #'stencil = ##f
}
improvisationOff = {
  \unset squashedPosition
  \revert NoteHead #'style
  \revert Accidental #'stencil
}


%% merging

mergeDifferentlyDottedOn =
  \override Staff.NoteCollision #'merge-differently-dotted = ##t
mergeDifferentlyDottedOff =
  \revert Staff.NoteCollision #'merge-differently-dotted
mergeDifferentlyHeadedOn =
  \override Staff.NoteCollision #'merge-differently-headed = ##t
mergeDifferentlyHeadedOff =
  \revert Staff.NoteCollision #'merge-differently-headed


%% numeric time signature

numericTimeSignature = \override Staff.TimeSignature #'style = #'()
defaultTimeSignature = \revert Staff.TimeSignature #'style


%% palm mutes

palmMuteOn =
#(define-music-function (parser location) ()
   (override-head-style 'NoteHead 'do))
palmMuteOff =
#(define-music-function (parser location) ()
   (revert-head-style 'NoteHead))
palmMute =
#(define-music-function (parser location note) (ly:music?)
    (style-note-heads 'NoteHead 'do note))


%% phrasing slurs

% directions
phrasingSlurUp      = \override PhrasingSlur #'direction = #UP
phrasingSlurDown    = \override PhrasingSlur #'direction = #DOWN
phrasingSlurNeutral = \revert PhrasingSlur #'direction

% dash-patterns (make-simple-dash-definition defined at top of file)
phrasingSlurDashPattern =
#(define-music-function (parser location dash-fraction dash-period)
  (number? number?)
  #{
     \override PhrasingSlur #'dash-definition =
       $(make-simple-dash-definition dash-fraction dash-period)
  #})
phrasingSlurDashed =
  \override PhrasingSlur #'dash-definition = #'((0 1 0.4 0.75))
phrasingSlurDotted =
  \override PhrasingSlur #'dash-definition = #'((0 1 0.1 0.75))
phrasingSlurHalfDashed =
  \override PhrasingSlur #'dash-definition = #'((0 0.5 0.4 0.75)
						(0.5 1 1 1))
phrasingSlurHalfSolid =
  \override PhrasingSlur #'dash-definition = #'((0 0.5 1 1)
						(0.5 1 0.4 0.75))
phrasingSlurSolid =
  \revert PhrasingSlur #'dash-definition


%% point and click

pointAndClickOn  =
#(define-music-function (parser location) ()
   (ly:set-option 'point-and-click #t)
   (make-music 'SequentialMusic 'void #t))
pointAndClickOff =
#(define-music-function (parser location) ()
   (ly:set-option 'point-and-click #f)
   (make-music 'SequentialMusic 'void #t))


%% predefined fretboards

predefinedFretboardsOff =
  \set FretBoards.predefinedDiagramTable = ##f
predefinedFretboardsOn =
  \set FretBoards.predefinedDiagramTable = #fretboard-table


%% shape note heads

aikenHeads      = \set shapeNoteStyles = #'#(do re mi fa #f la ti)
sacredHarpHeads = \set shapeNoteStyles = #'#(fa #f la fa #f la mi)


%% shifts

shiftOn   = \override NoteColumn #'horizontal-shift = #1
shiftOnn  = \override NoteColumn #'horizontal-shift = #2
shiftOnnn = \override NoteColumn #'horizontal-shift = #3
shiftOff  = \revert NoteColumn #'horizontal-shift


%% slurs

% directions
slurUp         = \override Slur #'direction = #UP
slurDown       = \override Slur #'direction = #DOWN
slurNeutral    = \revert Slur #'direction

% dash-patterns (make-simple-dash-definition defined at top of file)
slurDashPattern =
#(define-music-function (parser location dash-fraction dash-period)
  (number? number?)
  #{
     \override Slur #'dash-definition =
       $(make-simple-dash-definition dash-fraction dash-period)
  #})
slurDashed     = \override Slur #'dash-definition = #'((0 1 0.4 0.75))
slurDotted     = \override Slur #'dash-definition = #'((0 1 0.1 0.75))
slurHalfDashed = \override Slur #'dash-definition = #'((0 0.5 0.4 0.75)
						       (0.5 1 1 1))
slurHalfSolid  = \override Slur #'dash-definition = #'((0 0.5 1 1)
						       (0.5 1 0.4 0.75))
slurSolid      = \revert Slur #'dash-definition


%% staff switches

showStaffSwitch = \set followVoice = ##t
hideStaffSwitch = \set followVoice = ##f


%% stems

stemUp      = \override Stem #'direction = #UP
stemDown    = \override Stem #'direction = #DOWN
stemNeutral = \revert Stem #'direction


%% tablature

% switch to full notation
tabFullNotation = {
  % time signature
  \revert TabStaff.TimeSignature #'stencil
  % stems (the half note gets a double stem)
  \revert Stem #'length
  \revert Stem #'no-stem-extend
  \revert Stem #'flag-style
  \revert Stem #'details
  \revert Stem #'transparent
  \override TabVoice.Stem #'stencil = #tabvoice::draw-double-stem-for-half-notes
  autoBeaming = ##t
  \revert NoteColumn #'ignore-collision
  % beams, dots
  \revert TabVoice.Beam #'stencil
  \revert TabVoice.Dots #'stencil
  \revert TabVoice.Tie #'stencil
  \revert TabVoice.Tie #'after-line-breaking
  \revert TabVoice.RepeatTie #'stencil
  \revert TabVoice.RepeatTie #'after-line-breaking
  \revert TabVoice.LaissezVibrerTie #'stencil
  \revert TabVoice.Slur #'stencil
  \revert PhrasingSlur #'stencil
  % tuplet stuff
  \revert TabVoice.TupletBracket #'stencil
  \revert TabVoice.TupletNumber #'stencil
  % dynamic signs
  \revert DynamicText #'transparent
  \revert DynamicTextSpanner #'stencil
  \revert TabVoice.DynamicTextSpanner #'stencil
  \revert TabVoice.Hairpin #'transparent
  % rests
  \revert TabVoice.Rest #'stencil
  \revert TabVoice.MultiMeasureRest #'stencil
  % markups etc.
  \revert TabVoice.Script #'stencil
  \revert TabVoice.TextScript #'stencil
  \revert TabStaff.Arpeggio #'stencil
}

%tie/repeat tie behaviour
hideSplitTiedTabNotes = {
  \override TabVoice.TabNoteHead #'(details tied-properties break-visibility) = #all-invisible
  \override TabVoice.TabNoteHead #'(details tied-properties parenthesize) = ##f
  \override TabVoice.TabNoteHead #'(details repeat-tied-properties note-head-visible) = ##f
  \override TabVoice.TabNoteHead #'(details repeat-tied-properties parenthesize) = ##f
}

showSplitTiedTabNotes = {
  \override TabVoice.TabNoteHead #'(details tied-properties break-visibility) = #begin-of-line-visible
  \override TabVoice.TabNoteHead #'(details tied-properties parenthesize) = ##t
  \override TabVoice.TabNoteHead #'(details repeat-tied-properties note-head-visible) = ##t
  \override TabVoice.TabNoteHead #'(details repeat-tied-properties parenthesize) = ##t
}

%% text length

textLengthOn = {
  \override TextScript #'extra-spacing-width = #'(0 . 0)
  \override TextScript #'extra-spacing-height = #'(-inf.0 . +inf.0)
}
textLengthOff = {
  \override TextScript #'extra-spacing-width = #'(+inf.0 . -inf.0)
  \override TextScript #'extra-spacing-height = #'(0 . 0)
}


%% text spanners

textSpannerUp      = \override TextSpanner #'direction = #UP
textSpannerDown    = \override TextSpanner #'direction = #DOWN
textSpannerNeutral = \revert TextSpanner #'direction


%% ties

% directions
tieUp      = \override Tie #'direction = #UP
tieDown    = \override Tie #'direction = #DOWN
tieNeutral = \revert Tie #'direction

% dash-patterns (make-simple-dash-definition defined at top of file)
tieDashPattern =
#(define-music-function (parser location dash-fraction dash-period)
  (number? number?)
  #{
     \override Tie #'dash-definition =
       $(make-simple-dash-definition dash-fraction dash-period)
  #})
tieDashed     = \override Tie #'dash-definition = #'((0 1 0.4 0.75))
tieDotted     = \override Tie #'dash-definition = #'((0 1 0.1 0.75))
tieHalfDashed = \override Tie #'dash-definition = #'((0 0.5 0.4 0.75)
						     (0.5 1 1 1))
tieHalfSolid  = \override Tie #'dash-definition = #'((0 0.5 1 1)
						     (0.5 1 0.4 0.75))
tieSolid      = \revert Tie #'dash-definition


%% tuplets

tupletUp      = \override TupletBracket #'direction = #UP
tupletDown    = \override TupletBracket #'direction = #DOWN
tupletNeutral = \revert TupletBracket #'direction


%% voice properties

% dynamic ly:dir?  text script, articulation script ly:dir?
voiceOne   = #(context-spec-music (make-voice-props-set 0)  'Voice)
voiceTwo   = #(context-spec-music (make-voice-props-set 1)  'Voice)
voiceThree = #(context-spec-music (make-voice-props-set 2)  'Voice)
voiceFour  = #(context-spec-music (make-voice-props-set 3)  'Voice)
oneVoice   = #(context-spec-music (make-voice-props-revert) 'Voice)


%% voice styles

voiceOneStyle = {
  \override NoteHead #'style = #'diamond
  \override NoteHead #'color = #red
  \override Stem #'color = #red
  \override Beam #'color = #red
}
voiceTwoStyle = {
  \override NoteHead #'style = #'triangle
  \override NoteHead #'color = #blue
  \override Stem #'color = #blue
  \override Beam #'color = #blue
}
voiceThreeStyle = {
  \override NoteHead #'style = #'xcircle
  \override NoteHead #'color = #green
  \override Stem #'color = #green
  \override Beam #'color = #green
}
voiceFourStyle = {
  \override NoteHead #'style = #'cross
  \override NoteHead #'color = #magenta
  \override Stem #'color = #magenta
  \override Beam #'color = #magenta
}
voiceNeutralStyle = {
  \revert NoteHead #'style
  \revert NoteHead #'color
  \revert Stem #'color
  \revert Beam #'color
}


%% x notes

xNotesOn =
#(define-music-function (parser location) ()
   (override-head-style '(TabNoteHead NoteHead) 'cross))
xNotesOff =
#(define-music-function (parser location) ()
   (revert-head-style '(TabNoteHead NoteHead)))
xNote =
#(define-music-function (parser location note) (ly:music?)
   (style-note-heads '(TabNoteHead NoteHead) 'cross note))


%% dead notes (these need to come after "x notes")

% Define aliases of cross-head notes for specific purposes
deadNotesOn  = \xNotesOn
deadNotesOff = \xNotesOff
deadNote     = #xNote
