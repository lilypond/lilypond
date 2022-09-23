% property-init.ly
%%%% Predefined property setting commands.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 1997--2022 Jan Nieuwenhuizen <janneke@gnu.org>,
%%%%                          Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.23.4"

%% for dashed slurs, phrasing slurs, and ties
#(define (make-simple-dash-definition dash-fraction dash-period)
    (list (list 0 1 dash-fraction dash-period)))

%% common definition for all note head styles reverting
%% (palm mute, harmonics, dead notes, ...)
defaultNoteHeads = {
  \revert NoteHead.style
  \revert TabNoteHead.style
}

accidentalStyle =
#(define-music-function
   (style) (symbol-list?)
   (_i "Set accidental style to symbol list @var{style} in the form
@samp{piano-cautionary}.  If @var{style} has a form like
@samp{Staff.piano-cautionary}, the settings are applied to that
context.  Otherwise, the context defaults to @samp{Staff}, except for
piano styles, which use @samp{GrandStaff} as a context." )
   (case (length style)
    ((1) (set-accidental-style (car style)))
    ((2) (set-accidental-style (cadr style) (car style)))
    (else
     (ly:parser-error (G_ "not an accidental style")
      (*location*))
     (make-music 'Music))))

%% ambitus

ambitusAfter =
#(define-music-function (target) (symbol?)
  (_i "Move the ambitus after the break-align symbol @var{target}.")
  (make-apply-context
   (lambda (context)
     (define (move-ambitus order)
       (let* ((without-ambitus (delq 'ambitus order))
              (target-index (list-index (lambda (x) (eq? x target)) without-ambitus))
              (head (take without-ambitus (1+ target-index)))
              (tail (drop without-ambitus (1+ target-index))))
           (append head '(ambitus) tail)))
     (let* ((score (ly:context-find context 'Score))
            (grob-def (ly:context-grob-definition score 'BreakAlignment))
            (orders (vector->list (ly:assoc-get 'break-align-orders grob-def)))
            (new-orders (list->vector (map move-ambitus orders))))
       (ly:context-pushpop-property score 'BreakAlignment 'break-align-orders new-orders)))))

%% arpeggios

% For drawing vertical chord brackets with \arpeggio
% This is a shorthand for the value of the print-function property
% of either Staff.Arpeggio or PianoStaff.Arpeggio, depending whether
% cross-staff brackets are desired.

arpeggio = #(make-music 'ArpeggioEvent)
arpeggioArrowUp = {
  \revert Arpeggio.stencil
  \revert Arpeggio.X-extent
  \override Arpeggio.arpeggio-direction = #UP
}
arpeggioArrowDown = {
  \revert Arpeggio.stencil
  \revert Arpeggio.X-extent
  \override Arpeggio.arpeggio-direction = #DOWN
}
arpeggioNormal = {
  \revert Arpeggio.stencil
  \revert Arpeggio.X-extent
  \revert Arpeggio.arpeggio-direction
  \revert Arpeggio.dash-definition
}
arpeggioBracket = {
  \revert Arpeggio.X-extent
  \override Arpeggio.stencil = #ly:arpeggio::brew-chord-bracket
}
arpeggioParenthesis = {
  \override Arpeggio.stencil = #ly:arpeggio::brew-chord-slur
  \override Arpeggio.X-extent = #ly:grob::stencil-width
  \revert Arpeggio.dash-definition
}
arpeggioParenthesisDashed = {
  \override Arpeggio.stencil = #ly:arpeggio::brew-chord-slur
  \override Arpeggio.X-extent = #ly:grob::stencil-width
  \override Arpeggio.dash-definition = #'((0 1 0.4 0.75))
}


%% auto beaming

autoBeamOn  = \set autoBeaming = ##t
autoBeamOff = \set autoBeaming = ##f


%% balloon length

balloonLengthOn = {
  \override BalloonText.extra-spacing-width = #'(0 . 0)
  \override BalloonText.extra-spacing-height = #'(-inf.0 . +inf.0)
}
balloonLengthOff = {
  \override BalloonText.extra-spacing-width = #'(+inf.0 . -inf.0)
  \override BalloonText.extra-spacing-height = #'(0 . 0)
}


%% bar lines

defineBarLine =
#(define-void-function
   (bar glyph-list) (string? list?)
   (_i "Define bar line settings for bar line @var{bar}.  The list
     @var{glyph-list} must have three entries which define substitute
     glyphs for the end of a line, the beginning of a line, and a span
     bar, respectively.  The substitute glyphs may be either strings
     or booleans: @code{#t} calls for the same value as
     @var{bar} and @code{#f} calls for no glyph." )
  (if (not (= (length glyph-list) 3))
      (ly:error (G_ "Argument list for bar '~a' must have three components.") bar)
      (define-bar-line bar
                       (car glyph-list)
                       (cadr glyph-list)
                       (caddr glyph-list))))


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
  \override Staff.BassFigureAlignmentPositioning.direction = #DOWN
bassFigureStaffAlignmentUp =
  \override Staff.BassFigureAlignmentPositioning.direction = #UP
bassFigureStaffAlignmentNeutral =
  \revert Staff.BassFigureAlignmentPositioning.direction


%% bend-spanner
skipNCs = \override NoteColumn.bend-me = ##f
skipNC = \once \skipNCs
endSkipNCs = \revert NoteColumn.bend-me


%% cadenzas

cadenzaOn  = \set Timing.timing = ##f

cadenzaOff = \set Timing.timing = ##t

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

medianChordGridStyle = {
  \override ChordSquare.measure-division-chord-placement-alist =
    #median-measure-division-chord-placement-alist
  \override ChordSquare.measure-division-lines-alist =
    #median-measure-division-lines-alist
}

%% compressEmptyMeasures

compressEmptyMeasures = \set Score.skipBars = ##t
expandEmptyMeasures   = \set Score.skipBars = ##f


%% dots

dotsUp      = \override Dots.direction = #UP
dotsDown    = \override Dots.direction = #DOWN
dotsNeutral = \revert Dots.direction


%% dynamics

dynamicUp = {
  \override DynamicText.direction = #UP
  \override DynamicLineSpanner.direction = #UP
}
dynamicDown = {
  \override DynamicText.direction = #DOWN
  \override DynamicLineSpanner.direction = #DOWN
}
dynamicNeutral = {
  \revert DynamicText.direction
  \revert DynamicLineSpanner.direction
}


%% easy heads

easyHeadsOn = {
  \override NoteHead.stencil = #note-head::brew-ez-stencil
  \override NoteHead.font-family = #'sans
  \override NoteHead.font-series = #'bold
}
easyHeadsOff = {
  \revert NoteHead.stencil
  \revert NoteHead.font-family
  \revert NoteHead.font-series
}


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

harmonicsOn =
#(define-music-function () ()
  (_i "Set the default note head style to a diamond-shaped style.")
  (context-spec-music
   (override-head-style '(NoteHead TabNoteHead) 'harmonic) 'Bottom))
harmonicsOff = \defaultNoteHeads
harmonicNote =
#(define-music-function (note) (ly:music?)
   (_i "Print @var{note} with a diamond-shaped note head.")
   (style-note-heads 'NoteHead 'harmonic note))

%% hideNotes

hideNotes = {
  % hide notes, accidentals, etc.
  \override Dots.transparent = ##t
  \override NoteHead.transparent = ##t
  \override NoteHead.no-ledgers = ##t
  % assume that any Beam inherits transparency from its parent Stem
  \override Stem.transparent = ##t
  \override Accidental.transparent = ##t
  \override Rest.transparent = ##t
  \override TabNoteHead.transparent = ##t
}
unHideNotes = {
  \revert Accidental.transparent
  \revert Stem.transparent
  \revert NoteHead.transparent
  \revert NoteHead.no-ledgers
  \revert Dots.transparent
  \revert Rest.transparent
  \revert TabNoteHead.transparent
}


%% improvisation

improvisationOn = {
  \set squashedPosition = #0
  \override NoteHead.style = #'slash
  \override TabNoteHead.style = #'slash
  \override Accidental.stencil = ##f
  \override AccidentalCautionary.stencil = ##f
}
improvisationOff = {
  \unset squashedPosition
  \revert NoteHead.style
  \revert TabNoteHead.style
  \revert Accidental.stencil
  \revert AccidentalCautionary.stencil
}

%% incipit

incipit =
#(define-music-function (incipit-music) (ly:music?)
  (_i "Output @var{incipit-music} before the main staff as an indication of
    its appearance in the original music.")
  #{
    \once \override Staff.InstrumentName.stencil =
      #(lambda (grob)
        (let* ((instrument-name (ly:grob-property grob 'long-text))
               (align-x (ly:grob-property grob 'self-alignment-X 0))
               (align-y (ly:grob-property grob 'self-alignment-Y 0)))
        (set! (ly:grob-property grob 'long-text)
          #{ \markup {
            \score
            {
              \new MensuralStaff \with {
                \override InstrumentName.self-alignment-X = #align-x
                \override InstrumentName.self-alignment-Y = #align-y
                instrumentName = #instrument-name
              }
              {
                $incipit-music
              }
              \layout {
                $(ly:grob-layout grob)
                indent-incipit-default = 15\mm
                line-width = #(primitive-eval
                  '(or (false-if-exception indent)
                    indent-incipit-default))
                indent = #(primitive-eval
                           '(or (false-if-exception (- line-width incipit-width))
                            (* 0.5 line-width)))
                ragged-right = ##f
                ragged-last = ##f
                system-count = 1
              }
            }
            }
          #})
          (set! (ly:grob-property grob 'self-alignment-Y) #f)
          ;; Do 'self-alignment-X RIGHT only for the first InstrumentName, which
          ;; actually is the incipit. Otherwise self-alignment-X for the
          ;; shortInstrumentName is not longer settable.
          (let ((parts (ly:spanner-broken-into (ly:grob-original grob))))
            (if (and (pair? parts) (equal? grob (car parts)))
                (ly:grob-set-property! grob 'self-alignment-X RIGHT)))
          (system-start-text::print grob)))
  #}
)

%% kievan
kievanOn = {
 \override NoteHead.style = #'kievan
 \override Stem.X-offset = #stem::kievan-offset-callback
 \override Stem.stencil = ##f
 \override Flag.stencil = ##f
 \override Rest.style = #'mensural
 \override Accidental.alteration-glyph-name-alist = #alteration-kievan-glyph-name-alist
 \override Dots.style = #'kievan
 \override Slur.stencil = ##f
 \override Stem.length = #0.0
 \override Beam.positions = #beam::get-kievan-positions
 \override Beam.quantized-positions = #beam::get-kievan-quantized-positions
 \override NoteHead.duration-log = #note-head::calc-kievan-duration-log
}
kievanOff = {
 \revert NoteHead.style
 \revert Stem.X-offset
 \revert Stem.stencil
 \revert Rest.style
 \revert Accidental.alteration-glyph-name-alist
 \revert Dots.style
 \revert Slur.stencil
 \revert Flag.stencil
 \revert Stem.length
 \revert Beam.positions
 \revert Beam.quantized-positions
 \revert NoteHead.duration-log
}

%% line and page breaking controls

autoLineBreaksOff =
  \override Score.NonMusicalPaperColumn.line-break-permission = ##f

autoLineBreaksOn =
  \override Score.NonMusicalPaperColumn.line-break-permission = #'allow

autoPageBreaksOff = {
  \override Score.NonMusicalPaperColumn.page-break-permission = ##f
  % For unclear reasons, \autoPageBreaks{Off,On} do not affect the column
  % at the point they are placed, only the following columns.  FIXME:
  % this is inconsistent.  Should it be changed? --JeanAS
  \once \revert Score.NonMusicalPaperColumn.page-break-permission
}

autoPageBreaksOn = {
  \override Score.NonMusicalPaperColumn.page-break-permission = #'allow
  \once \revert Score.NonMusicalPaperColumn.page-break-permission
}

autoBreaksOff = { \autoLineBreaksOff \autoPageBreaksOff }
autoBreaksOn = { \autoLineBreaksOn \autoPageBreaksOn }


%% merging

mergeDifferentlyDottedOn =
  \override Staff.NoteCollision.merge-differently-dotted = ##t
mergeDifferentlyDottedOff =
  \revert Staff.NoteCollision.merge-differently-dotted
mergeDifferentlyHeadedOn =
  \override Staff.NoteCollision.merge-differently-headed = ##t
mergeDifferentlyHeadedOff =
  \revert Staff.NoteCollision.merge-differently-headed


%% numeric time signature

numericTimeSignature = \override Timing.TimeSignature.style = #'numbered
defaultTimeSignature = \revert Timing.TimeSignature.style


%% palm mutes

palmMuteOn =
#(define-music-function () ()
  (_i "Set the default note head style to a triangle-shaped style.")
  (context-spec-music
   (override-head-style 'NoteHead 'do) 'Bottom))
palmMuteOff = \defaultNoteHeads
palmMute =
#(define-music-function (note) (ly:music?)
   (_i "Print @var{note} with a triangle-shaped note head.")
   (style-note-heads 'NoteHead 'do note))

%% part combiner

partCombineForce =
#(define-music-function (type) ((symbol?))
   (_i "Override the part-combiner.")
   (if type (propertySet 'partCombineForced type)
       (propertyUnset 'partCombineForced)))

partCombineApart = \partCombineForce #'apart
partCombineChords = \partCombineForce #'chords
partCombineUnisono = \partCombineForce #'unisono
partCombineSoloI = \partCombineForce #'solo1
partCombineSoloII = \partCombineForce #'solo2
partCombineAutomatic = \partCombineForce \default


%% phrasing slurs

% directions
phrasingSlurUp      = \override PhrasingSlur.direction = #UP
phrasingSlurDown    = \override PhrasingSlur.direction = #DOWN
phrasingSlurNeutral = \revert PhrasingSlur.direction

% dash-patterns (make-simple-dash-definition defined at top of file)
phrasingSlurDashPattern =
#(define-music-function (dash-fraction dash-period)
   (number? number?)
   (_i "Set up a custom style of dash pattern for @var{dash-fraction} ratio of
line to space repeated at @var{dash-period} interval for phrasing slurs.")
  #{
     \override PhrasingSlur.dash-definition =
       $(make-simple-dash-definition dash-fraction dash-period)
  #})
phrasingSlurDashed =
  \override PhrasingSlur.dash-definition = #'((0 1 0.4 0.75))
phrasingSlurDotted =
  \override PhrasingSlur.dash-definition = #'((0 1 0.1 0.75))
phrasingSlurHalfDashed =
  \override PhrasingSlur.dash-definition = #'((0 0.5 0.4 0.75)
                                                (0.5 1 1 1))
phrasingSlurHalfSolid =
  \override PhrasingSlur.dash-definition = #'((0 0.5 1 1)
                                                (0.5 1 0.4 0.75))
phrasingSlurSolid =
  \revert PhrasingSlur.dash-definition


%% point and click

pointAndClickOn  =
#(define-void-function () ()
   (_i "Enable generation of code in final-format (e.g. pdf) files to reference the
originating lilypond source statement;
this is helpful when developing a score but generates bigger final-format files.")
   (ly:set-option 'point-and-click #t))

pointAndClickOff =
#(define-void-function () ()
   (_i "Suppress generating extra code in final-format (e.g. pdf) files to point
back to the lilypond source statement.")
   (ly:set-option 'point-and-click #f))

pointAndClickTypes =
#(define-void-function (types) (symbol-list-or-symbol?)
  (_i "Set a type or list of types (such as @code{#'note-event}) for which point-and-click info is generated.")
  (ly:set-option 'point-and-click types))

%% predefined fretboards

predefinedFretboardsOff =
  \set predefinedDiagramTable = ##f
predefinedFretboardsOn =
  \set predefinedDiagramTable = #default-fret-table


%% shape note heads

aikenHeads      = \set shapeNoteStyles = ##(do re miMirror fa sol la ti)
aikenHeadsMinor = \set shapeNoteStyles = ##(la ti do re miMirror fa sol)
aikenThinHeads =
  \set shapeNoteStyles = ##(doThin reThin miThin faThin sol laThin tiThin)
aikenThinHeadsMinor =
  \set shapeNoteStyles = ##(laThin tiThin doThin reThin miThin faThin sol)
funkHeads =
  \set shapeNoteStyles = ##(doFunk reFunk miFunk faFunk solFunk laFunk tiFunk)
funkHeadsMinor =
  \set shapeNoteStyles = ##(laFunk tiFunk doFunk reFunk miFunk faFunk solFunk)
sacredHarpHeads = \set shapeNoteStyles = ##(fa sol la fa sol la mi)
sacredHarpHeadsMinor = \set shapeNoteStyles = ##(la mi fa sol la fa sol)
southernHarmonyHeads =
  \set shapeNoteStyles = ##(faThin sol laThin faThin sol laThin miThin)
southernHarmonyHeadsMinor =
  \set shapeNoteStyles = ##(laThin miThin faThin sol laThin faThin sol)
walkerHeads =
  \set shapeNoteStyles = ##(doWalker reWalker miWalker faWalker solFunk laWalker tiWalker)
walkerHeadsMinor =
  \set shapeNoteStyles = ##(laWalker tiWalker doWalker reWalker miWalker faWalker solFunk)


%% shifts

shiftOff  = \override NoteColumn.horizontal-shift = #0
shiftOn   = \override NoteColumn.horizontal-shift = #1
shiftOnn  = \override NoteColumn.horizontal-shift = #2
shiftOnnn = \override NoteColumn.horizontal-shift = #3


%% slurs

% directions
slurUp         = \override Slur.direction = #UP
slurDown       = \override Slur.direction = #DOWN
slurNeutral    = \revert Slur.direction

% dash-patterns (make-simple-dash-definition defined at top of file)
slurDashPattern =
#(define-music-function (dash-fraction dash-period)
  (number? number?)
  (_i "Set up a custom style of dash pattern for @var{dash-fraction}
ratio of line to space repeated at @var{dash-period} interval for slurs.")
  #{
     \override Slur.dash-definition =
       $(make-simple-dash-definition dash-fraction dash-period)
  #})
slurDashed     = \override Slur.dash-definition = #'((0 1 0.4 0.75))
slurDotted     = \override Slur.dash-definition = #'((0 1 0.1 0.75))
slurHalfDashed = \override Slur.dash-definition = #'((0 0.5 0.4 0.75)
                                                       (0.5 1 1 1))
slurHalfSolid  = \override Slur.dash-definition = #'((0 0.5 1 1)
                                                       (0.5 1 0.4 0.75))
slurSolid      = \revert Slur.dash-definition


%% staff highlights

staffHighlight =
#(define-music-function (color) (color?)
  (_i "Start a highlight with the specified color.")
  (make-music 'StaffHighlightEvent
              'span-direction START
              'color color))

stopStaffHighlight =
#(make-music 'StaffHighlightEvent 'span-direction STOP)

%% staff switches

showStaffSwitch = \set followVoice = ##t
hideStaffSwitch = \set followVoice = ##f


%% stems

stemUp      = \override Stem.direction = #UP
stemDown    = \override Stem.direction = #DOWN
stemNeutral = \revert Stem.direction


%% string numbers

romanStringNumbers  = {
  \override StringNumber.number-type = #'roman-upper
  \override StringNumber.stencil = #ly:text-interface::print
  \override StringNumber.font-encoding = #'latin1
  \override StringNumber.font-shape = #'italic
}
arabicStringNumbers = {
  \revert StringNumber.number-type
  \revert StringNumber.stencil
  \revert StringNumber.font-encoding
  \revert StringNumber.font-shape
}


%% tablature

% switch to full notation
tabFullNotation = {
  % time signature
  \revert TabStaff.TimeSignature.stencil
  % stems (the half note gets a double stem)
  \revert TabStaff.Stem.length
  \revert TabStaff.Stem.no-stem-extend
  \revert TabStaff.Flag.style
  \revert TabStaff.Stem.details
  \revert TabStaff.Stem.stencil
  \revert TabStaff.Flag.stencil
  \override TabStaff.Stem.stencil = #tabvoice::draw-double-stem-for-half-notes
  \override TabStaff.Stem.X-extent = #tabvoice::make-double-stem-width-for-half-notes
  \set TabStaff.autoBeaming = ##t
  \revert TabStaff.NoteColumn.ignore-collision
  % beams, dots
  \revert TabStaff.Beam.stencil
  \revert TabStaff.StemTremolo.stencil
  \revert TabStaff.Dots.stencil
  \revert TabStaff.Tie.stencil
  \revert TabStaff.Tie.after-line-breaking
  \revert TabStaff.RepeatTie.stencil
  \revert TabStaff.RepeatTie.after-line-breaking
  \revert TabStaff.LaissezVibrerTie.stencil
  \revert TabStaff.Slur.control-points
  \revert TabStaff.PhrasingSlur.stencil
  % tuplet stuff
  \revert TabStaff.TupletBracket.stencil
  \revert TabStaff.TupletNumber.stencil
  % dynamic signs
  \revert TabStaff.DynamicText.stencil
  \revert TabStaff.DynamicTextSpanner.stencil
  \revert TabStaff.DynamicTextSpanner.stencil
  \revert TabStaff.Hairpin.stencil
  % rests
  \revert TabStaff.Rest.stencil
  \revert TabStaff.MultiMeasureRest.stencil
  \revert TabStaff.MultiMeasureRestNumber.stencil
  \revert TabStaff.MultiMeasureRestScript.stencil
  \revert TabStaff.MultiMeasureRestText.stencil
  % markups etc.
  \revert TabStaff.Glissando.stencil
  \revert TabStaff.Script.stencil
  \revert TabStaff.TextScript.stencil
  \revert TabStaff.TextSpanner.stencil
  \revert TabStaff.Arpeggio.stencil
  \revert TabStaff.NoteColumn.ignore-collision
}

%tie/repeat tie behavior
hideSplitTiedTabNotes = {
  \override TabVoice.TabNoteHead.details.tied-properties.break-visibility = #all-invisible
  \override TabVoice.TabNoteHead.details.tied-properties.parenthesize = ##f
  \override TabVoice.TabNoteHead.details.repeat-tied-properties.note-head-visible = ##f
  \override TabVoice.TabNoteHead.details.repeat-tied-properties.parenthesize = ##f
}

showSplitTiedTabNotes = {
  \override TabVoice.TabNoteHead.details.tied-properties.break-visibility = #begin-of-line-visible
  \override TabVoice.TabNoteHead.details.tied-properties.parenthesize = ##t
  \override TabVoice.TabNoteHead.details.repeat-tied-properties.note-head-visible = ##t
  \override TabVoice.TabNoteHead.details.repeat-tied-properties.parenthesize = ##t
}

%% text length

textLengthOn = {
  % 0.4 staff-space between adjacent texts
  \override TextScript.extra-spacing-width = #'(-0.0 . 0.4)
  \override TextScript.extra-spacing-height = #'(-inf.0 . +inf.0)
}

textLengthOff = {
  \override TextScript.extra-spacing-width = #'(+inf.0 . -inf.0)
  \override TextScript.extra-spacing-height = #'(0 . 0)
}

markLengthOn = {
  \override Score.MetronomeMark.extra-spacing-width = #'(0 . 1.0)
  \override Score.RehearsalMark.extra-spacing-width = #'(-0.5 . 0.5)
  \override Score.TextMark.extra-spacing-width = #'(-0.5 . 0.5)
  % Raise as much as four staff-spaces before pushing notecolumns right
  \override Score.MetronomeMark.extra-spacing-height = #'(4 . 4)
  \override Score.RehearsalMark.extra-spacing-height = #'(4 . 4)
  \override Score.TextMark.extra-spacing-height = #'(4 . 4)
}

markLengthOff = {
  \override Score.MetronomeMark.extra-spacing-width = #'(+inf.0 . -inf.0)
  \override Score.RehearsalMark.extra-spacing-width = #'(+inf.0 . -inf.0)
  \override Score.TextMark.extra-spacing-width = #'(+inf.0 . -inf.0)
  \revert Score.MetronomeMark.extra-spacing-height
  \revert Score.RehearsalMark.extra-spacing-height
  \revert Score.TextMark.extra-spacing-height
}

%% text spanners

textSpannerUp      = \override TextSpanner.direction = #UP
textSpannerDown    = \override TextSpanner.direction = #DOWN
textSpannerNeutral = \revert TextSpanner.direction


%% ties

% directions
tieUp      = \override Tie.direction = #UP
tieDown    = \override Tie.direction = #DOWN
tieNeutral = \revert Tie.direction

% dash-patterns (make-simple-dash-definition defined at top of file)
tieDashPattern =
#(define-music-function (dash-fraction dash-period)
  (number? number?)
  (_i "Set up a custom style of dash pattern for @var{dash-fraction}
ratio of line to space repeated at @var{dash-period} interval for ties.")
  #{
     \override Tie.dash-definition =
       $(make-simple-dash-definition dash-fraction dash-period)
  #})
tieDashed     = \override Tie.dash-definition = #'((0 1 0.4 0.75))
tieDotted     = \override Tie.dash-definition = #'((0 1 0.1 0.75))
tieHalfDashed = \override Tie.dash-definition = #'((0 0.5 0.4 0.75)
                                                     (0.5 1 1 1))
tieHalfSolid  = \override Tie.dash-definition = #'((0 0.5 1 1)
                                                     (0.5 1 0.4 0.75))
tieSolid      = \revert Tie.dash-definition


%% tuplets

tupletUp      = \override TupletBracket.direction = #UP
tupletDown    = \override TupletBracket.direction = #DOWN
tupletNeutral = \revert TupletBracket.direction


%% voice properties

% dynamic ly:dir?  text script, articulation script ly:dir?
voiceOne   = #(context-spec-music (make-voice-props-set 0)  'Voice)
voiceTwo   = #(context-spec-music (make-voice-props-set 1)  'Voice)
voiceThree = #(context-spec-music (make-voice-props-set 2)  'Voice)
voiceFour  = #(context-spec-music (make-voice-props-set 3)  'Voice)
oneVoice   = #(context-spec-music (make-voice-props-revert) 'Voice)


%% voice styles

voiceOneStyle = {
  \override NoteHead.style = #'diamond
  \override NoteHead.color = #red
  \override Stem.color = #red
  \override Beam.color = #red
}
voiceTwoStyle = {
  \override NoteHead.style = #'triangle
  \override NoteHead.color = #blue
  \override Stem.color = #blue
  \override Beam.color = #blue
}
voiceThreeStyle = {
  \override NoteHead.style = #'xcircle
  \override NoteHead.color = #green
  \override Stem.color = #green
  \override Beam.color = #green
}
voiceFourStyle = {
  \override NoteHead.style = #'cross
  \override NoteHead.color = #magenta
  \override Stem.color = #magenta
  \override Beam.color = #magenta
}
voiceNeutralStyle = {
  \revert NoteHead.style
  \revert NoteHead.color
  \revert Stem.color
  \revert Beam.color
}


%% volta brackets

allowVoltaHook =
#(define-void-function (bar) (string?)
  (_i "Allow the volta bracket hook being drawn over bar line @var{bar}.")
                       (allow-volta-hook bar))


%% vowel transitions

vowelTransition = #(make-music 'VowelTransitionEvent)


%% x notes

#(define (cross-style grob)
;; Returns the symbol 'cross to set the 'style-property for (Tab-)NoteHead.
;; If the current text-font doesn't contain the glyph set 'font-name to '()
;; and 'font-family to 'feta.
;; If 'feta is replaced by another music-font without cross-style-glyphs
;; note-head.cc throws a warning and no visual output happens.
  (let* ((layout (ly:grob-layout grob))
         (props (ly:grob-alist-chain grob))
         (font (ly:paper-get-font layout props))
         (font-unknown? (string=? (ly:font-name font) "unknown")))
    (if font-unknown?
        (begin
          (ly:grob-set-property! grob 'font-name '())
          (ly:grob-set-property! grob 'font-family 'feta)))
    'cross))

%% Set the default note head style to a cross-shaped style.
xNotesOn = {
  \temporary \override NoteHead.style = #cross-style
  \temporary \override TabNoteHead.style = #cross-style
}

xNotesOff = \defaultNoteHeads

xNote =
#(define-music-function (note) (ly:music?)
  (_i "Print @var{note} with a cross-shaped note head.")
   (if (eq? (ly:music-property note 'name) 'NoteEvent)
       #{ \tweak style #cross-style $note #}
       #{ \xNotesOn $note \xNotesOff #}))

%% dead notes (these need to come after "x notes")

% Define aliases of cross-head notes for specific purposes
deadNotesOn  = \xNotesOn
deadNotesOff = \xNotesOff
deadNote     = #xNote
