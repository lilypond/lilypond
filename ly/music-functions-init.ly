%%%% -*- Mode: Scheme -*-

%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2003--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%                          Jan Nieuwenhuizen <janneke@gnu.org>
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

\version "2.21.0"


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this file is alphabetically sorted.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% need SRFI-1 for filter; optargs for lambda*
#(use-modules (srfi srfi-1)
              (ice-9 optargs))

absolute =
#(define-music-function (music)
   (ly:music?)
   (_i "Make @var{music} absolute.  This does not actually change the
music itself but rather hides it from surrounding @code{\\relative}
and @code{\\fixed} commands.")
   (make-music 'RelativeOctaveMusic 'element music))

acciaccatura =
#(def-grace-function startAcciaccaturaMusic stopAcciaccaturaMusic
   (_i "Create an acciaccatura from the following music expression"))

%% keep these two together
instrument-definitions = #'()
addInstrumentDefinition =
#(define-void-function
   (name lst) (string? list?)
   (_i "Create instrument @var{name} with properties @var{list}.

This function is deprecated.")
   (set! instrument-definitions (acons name lst instrument-definitions)))

addQuote =
#(define-void-function (name music) (string? ly:music?)
   (_i "Define @var{music} as a quotable music expression named
@var{name}")
   (add-quotable name music))

after =
#(define-music-function (delta ev mus) (ly:duration? ly:music? ly:music?)
  (_i "Add music @var{ev} (usually a post-event) with a delay of @var{delta}
after the onset of @var{mus}.")
  (define (empty-chord? m)
     "Checks whether m is an empty chord <>."
     (and (music-is-of-type? m 'event-chord)
          (null? (ly:music-property m 'elements))
          (null? (ly:music-property m 'duration))))
  (if (and (not (empty-chord? mus))
           (ly:moment<?
            (ly:music-length mus)
            (ly:moment-add (ly:music-length ev) (ly:duration-length delta))))
      (ly:warning (G_ "\\after expression longer than main music argument.")))
  #{ \context Bottom << { \skip $delta <> $ev } #mus >> #})

%% keep these two together
afterGraceFraction = 3/4
afterGrace =
#(define-music-function (fraction main grace) ((scale?) ly:music? ly:music?)
   (_i "Create @var{grace} note(s) after a @var{main} music expression.

The musical position of the grace expression is after a
given fraction of the main note's duration has passed.  If
@var{fraction} is not specified as first argument, it is taken from
@code{afterGraceFraction} which has a default value of @code{3/4}.")
   (let* ((factor
            (scale->factor (or fraction
                               (ly:parser-lookup 'afterGraceFraction))))
          (delta
            (* factor (ly:moment-main (ly:music-length main))))
          (grace-music
            (make-music 'GraceMusic 'element grace)))
     (if (> factor 1)
         (ly:warning (G_ "\\afterGrace exceeds duration of main argument.")))
     ;;; Despite the similarity of definitions, don't reduce this to
     ;;; an application of \after. For \afterGrace, the arguments are
     ;;; input in visual order, hence they should be used in that order
     ;;; to ensure
     ;;;   \relative \afterGrace c'2 d8
     ;;; gives the expected result.
     #{
       \context Bottom << #main { \skip 1*$delta #grace-music } >>
     #}))


%% music identifiers not allowed at top-level,
%% so this is a music-function instead.
allowPageTurn =
#(define-music-function () ()
   (_i "Allow a page turn. May be used at toplevel (ie between scores or
markups), or inside a score.")
   (make-music 'EventChord
               'page-marker #t
               'page-turn-permission 'allow
               'elements (list (make-music 'PageTurnEvent
                                           'break-permission 'allow))))

alterBroken =
#(define-music-function (property arg target)
  (key-list-or-symbol? list? key-list-or-music?)
  (_i "Override @var{property} for pieces of broken spanner @var{target}
with values @var{arg}.  @var{target} may either be music in the form of
a starting spanner event, or a symbol list in the form
@samp{Context.Grob} or just @samp{Grob}.  Iff @var{target} is in the
form of a spanner event, @var{property} may also have the form
@samp{Grob.property} for specifying a directed tweak.")
  (if (ly:music? target)
      (if (or (eqv? (ly:music-property target 'span-direction) START)
              (music-is-of-type? target 'tie-event))
          (tweak property (value-for-spanner-piece property arg) target)
          (begin
            (ly:music-warning target (G_ "not a spanner"))
            target))
      (propertyOverride (append target (if (symbol? property)
                                           (list property)
                                           property))
                        (value-for-spanner-piece property arg))))

appendToTag =
#(define-music-function (tag more music)
   (symbol? ly:music? ly:music?)
   (_i "Append @var{more} to the back of music tagged with @var{tag}.  A
@code{post-event} can be added to the articulations of rhythmic events
or chords; other expressions may be added to chords, sequential or
simultaneous music.")
   (define (add-right m more)
     (cond
      ((or (music-is-of-type? m 'sequential-music)
           (music-is-of-type? m 'simultaneous-music))
       (if (ly:event? more)
           (begin
             (ly:music-warning m (G_ "\\appendToTag cannot append post-event"))
             (ly:music-message m (G_ "to this music")))
           (set! (ly:music-property m 'elements)
                 (append! (ly:music-property m 'elements) (list more)))))
      ((music-is-of-type? m 'event-chord)
       (cond ((ly:event? more)
              (set! (ly:music-property m 'elements)
                    (append! (ly:music-property m 'elements (list more)))))
             ((music-is-of-type? more 'rhythmic-event)
              (set! (ly:music-property m 'elements)
                    (call-with-values (lambda ()
                                        (break! ly:event?
                                                (ly:music-property m 'elements)))
                      (lambda (elts posts)
                        (append! elts (cons more posts))))))
             (else
              (ly:music-warning more (G_ "\\appendToTag cannot append this"))
              (ly:music-message m (G_ "to this event-chord")))))
      ((music-is-of-type? m 'rhythmic-event)
       (if (ly:event? more)
           (set! (ly:music-property m 'articulations)
                 (append! (ly:music-property m 'articulations) (list more)))
           (begin
             (ly:music-warning more (G_ "\\appendToTag cannot append this music"))
             (ly:music-message m (G_ "to this rhythmic-event")))))
      ((music-is-of-type? m 'music-wrapper-music)
       (add-right (ly:music-property m 'element) more))
      (else
       (ly:input-warning (*location*) (G_ "\\appendToTag failed:"))
       (ly:music-message m (G_ "No \\appendToTag destination")))))
   (music-map (lambda (m)
                (if (memq tag (ly:music-property m 'tags))
                    (add-right m more))
                m)
              music))

applyContext =
#(define-music-function (proc) (procedure?)
   (_i "Modify context properties with Scheme procedure @var{proc}.")
   (make-music 'ApplyContext
               'procedure proc))

applyMusic =
#(define-music-function (func music) (procedure? ly:music?)
   (_i"Apply procedure @var{func} to @var{music}.")
   (func music))

applyOutput =
#(define-music-function (target proc) (symbol-list-or-symbol? procedure?)
   (_i "Apply function @code{proc} to every layout object matched by
@var{target} which takes the form @code{Context} or @code{Context.Grob}.")
   (let ((p (check-grob-path target #:max 2)))
     (if p
         (make-music 'ApplyOutputEvent
                     'procedure proc
                     'context-type (car p)
                     (if (pair? (cdr p))
                         (list (cons 'symbol (cadr p)))
                         '()))
         (make-music 'Music))))

appoggiatura =
#(def-grace-function startAppoggiaturaMusic stopAppoggiaturaMusic
   (_i "Create an appoggiatura from @var{music}"))

% for regression testing purposes.
assertBeamQuant =
#(define-music-function (l r) (pair? pair?)
   (_i "Testing function: check whether the beam quants @var{l} and @var{r} are correct")
   (make-grob-property-override 'Beam 'positions (check-quant-callbacks l r)))

% for regression testing purposes.
assertBeamSlope =
#(define-music-function (comp) (procedure?)
   (_i "Testing function: check whether the slope of the beam is the same as @code{comp}")
   (make-grob-property-override 'Beam 'positions (check-slope-callbacks comp)))

autoChange =
#(define-music-function (pitch clef-1 clef-2 music)
  ((ly:pitch?) (ly:context-mod?) (ly:context-mod?) ly:music?)
  (_i "Make voices that switch between staves automatically.  As an option the
pitch where to switch staves may be specified.  The clefs for the staves are
optional as well.  Setting clefs works only for implicitly instantiated
staves.")
  (let ;; keep the contexts alive for the full duration
       ((skip (make-duration-of-length (ly:music-length music)))
        (clef-1 (or clef-1 #{ \with { \clef "treble" } #}))
        (clef-2 (or clef-2 #{ \with { \clef "bass" } #})))
    (make-simultaneous-music
     (list
      (descend-to-context (make-autochange-music music pitch) 'Staff
                          "up" clef-1)
      (context-spec-music (make-skip-music skip) 'Staff
                          "up" clef-1)
      (context-spec-music (make-skip-music skip) 'Staff
                          "down" clef-2)))))

balloonGrobText =
#(define-music-function (grob-name offset text)
   (symbol? number-pair? markup?)
   (_i "Attach @var{text} to @var{grob-name} at offset @var{offset}
 (use like @code{\\once})")
   (make-event-chord
    (list
     (make-music 'AnnotateOutputEvent
                 'symbol grob-name
                 'X-offset (car offset)
                 'Y-offset (cdr offset)
                 'text text))))

balloonText =
#(define-event-function (offset text) (number-pair? markup?)
   (_i "Attach @var{text} at @var{offset} (use like @code{\\tweak})")
   (make-music 'AnnotateOutputEvent
               'X-offset (car offset)
               'Y-offset (cdr offset)
               'text text))

bar =
#(define-music-function (type) (string?)
   (_i "Insert a bar line of type @var{type}, overriding any automatic
bar lines.")
   (make-music 'BarEvent 'bar-type type))

barNumberCheck =
#(define-music-function (n) (integer?)
   (_i "Print a warning if the current bar number is not @var{n}.")
   (make-music 'ApplyContext
               'procedure
               (lambda (c)
                 (if (not (ly:context-property c 'ignoreBarNumberChecks #f))
                     (let ((cbn (ly:context-property c 'currentBarNumber)))
                       (if (and (number? cbn) (not (= cbn n)))
                           (ly:input-warning (*location*)
                                             "Bar number is ~a; expected ~a"
                                             cbn n)))))))

beamExceptions =
#(define-scheme-function (music) (ly:music?)
   (_i "Extract a value suitable for setting
@code{Timing.beamExceptions} from the given pattern with explicit
beams in @var{music}.  A bar check @code{|} has to be used between
bars of patterns in order to reset the timing.")
   (extract-beam-exceptions music))

bendAfter =
#(define-event-function (delta) (real?)
   (_i "Create a fall or doit of pitch interval @var{delta}.")
   (make-music 'BendAfterEvent
               'delta-step delta))

%% BendSpanner convenience event functions
bendStartLevel =
#(define-event-function (idx mus)(index? ly:music?)
   (_i "Sets the @code{details.successive-level} of a @code{BendSpanner} to
@var{idx}.")
#{
  \tweak details.successive-level $idx
  $mus
#})

bendHold =
#(define-event-function (mus)(ly:music?)
   (_i "Sets the @code{'style} of a @code{BendSpanner} to @code{'hold}.")
#{
  \tweak style #'hold
  \tweak details.head-text-break-visibility ##(#t #t #f)
  $mus
#})

preBend =
#(define-event-function (mus)(ly:music?)
   (_i "Sets the @code{'style} of a @code{BendSpanner} to @code{'pre-bend}.")
#{
  \tweak style #'pre-bend
  \tweak details.head-text-break-visibility ##(#t #t #f)
  $mus
#})

preBendHold =
#(define-event-function (mus)(ly:music?)
   (_i "Sets the @code{'style} of a @code{BendSpanner} to
@code{'pre-bend-hold}.")
#{
  \tweak style #'pre-bend-hold
  \tweak details.head-text-break-visibility ##(#t #t #f)
  $mus
#})

bookOutputName =
#(define-void-function (newfilename) (string?)
   (_i "Direct output for the current book block to @var{newfilename}.")
   (set! (paper-variable #f 'output-filename) newfilename))

bookOutputSuffix =
#(define-void-function (newsuffix) (string?)
   (_i "Set the output filename suffix for the current book block to
@var{newsuffix}.")
   (set! (paper-variable #f 'output-suffix) newsuffix))

%% \breathe is defined as a music function rather than an event identifier to
%% ensure it gets useful input location information: as an event identifier,
%% it would have to be wrapped in an EventChord to prevent it from being
%% treated as a post_event by the parser
breathe =
#(define-music-function () ()
   (_i "Insert a breath mark.")
   (make-music 'BreathingEvent))

%% \caesura is defined as a music function rather than an event identifier to
%% ensure it gets useful input location information: as an event identifier,
%% it would have to be wrapped in an EventChord to prevent it from being
%% treated as a post_event by the parser
caesura =
#(define-music-function () ()
   (_i "Insert a caesura.")
   (make-music 'CaesuraEvent))

clef =
#(define-music-function (type) (string?)
   (_i "Set the current clef to @var{type}.")
   (make-clef-set type))

codaMark =
#(define-music-function (num) ((index?))
   (_i "Create a coda mark.  @var{num} may be 1@tie{}for the first
mark, 2@tie{}for the second, etc., or it may be @code{\\default} to
use the next number in sequence automatically.")
   (if num
       (make-music 'CodaMarkEvent 'label num)
       (make-music 'CodaMarkEvent)))

compoundMeter =
#(define-music-function (args) (pair?)
  (_i "Create compound time signatures. The argument is a Scheme list of
lists. Each list describes one fraction, with the last entry being the
denominator, while the first entries describe the summands in the
enumerator. If the time signature consists of just one fraction,
the list can be given directly, i.e. not as a list containing a single list.
For example, a time signature of (3+1)/8 + 2/4 would be created as
@code{\\compoundMeter #'((3 1 8) (2 4))}, and a time signature of (3+2)/8
as @code{\\compoundMeter #'((3 2 8))} or shorter
@code{\\compoundMeter #'(3 2 8)}.")
  (let* ((mlen (calculate-compound-measure-length args))
         (beat (calculate-compound-base-beat args))
         (beatGrouping (calculate-compound-beat-grouping args))
         (timesig (cons (ly:moment-main-numerator mlen)
                        (ly:moment-main-denominator mlen))))
  #{
    \once \override Timing.TimeSignature.stencil = #(lambda (grob)
      (grob-interpret-markup grob (make-compound-meter-markup args)))
    \set Timing.timeSignatureFraction = #timesig
    \set Timing.baseMoment = #beat
    \set Timing.beatStructure = #beatGrouping
    \set Timing.beamExceptions = #'()
    \set Timing.measureLength = #mlen
  #} ))

compressMMRests =
#(define-music-function (music) (ly:music?)
  (_i "Remove the empty bars created by multi-measure rests,
leaving just the first bar containing the MM rest itself.")
   (music-map
    (lambda (m)
      (if (eq? 'MultiMeasureRestMusic (ly:music-property m 'name))
          #{ \once \set Score.skipBars = ##t #m #}
          #{ #m #} ))
    music))

crossStaff =
#(define-music-function (notes) (ly:music?)
  (_i "Create cross-staff stems")
  #{
  \temporary \override Stem.cross-staff = #cross-staff-connect
  \temporary \override Flag.style = #'no-flag
  #notes
  \revert Stem.cross-staff
  \revert Flag.style
#})

cueClef =
#(define-music-function (type) (string?)
  (_i "Set the current cue clef to @var{type}.")
  (make-cue-clef-set type))

cueClefUnset =
#(define-music-function () ()
  (_i "Unset the current cue clef.")
  (make-cue-clef-unset))

cueDuring =
#(define-music-function
   (what dir main-music) (string? ly:dir? ly:music?)
   (_i "Insert contents of quote @var{what} corresponding to @var{main-music},
in a CueVoice oriented by @var{dir}.")
   (make-music 'QuoteMusic
               'element main-music
               'quoted-context-type 'CueVoice
               'quoted-context-id "cue"
               'quoted-music-name what
               'quoted-voice-direction dir))

cueDuringWithClef =
#(define-music-function
   (what dir clef main-music) (string? ly:dir? string? ly:music?)
   (_i "Insert contents of quote @var{what} corresponding to @var{main-music},
in a CueVoice oriented by @var{dir}.")
   (make-music 'QuoteMusic
               'element main-music
               'quoted-context-type 'CueVoice
               'quoted-context-id "cue"
               'quoted-music-name what
               'quoted-music-clef clef
               'quoted-voice-direction dir))



displayLilyMusic =
#(define-music-function (port music) ((output-port?) ly:music?)
   (_i "Display the LilyPond input representation of @var{music}
to @var{port}, defaulting to the console.")
   (let ((port (or port (current-output-port))))
     (newline port)
     (display-lily-music music port))
   music)

displayMusic =
#(define-music-function (port music) ((output-port?) ly:music?)
   (_i "Display the internal representation of @var{music} to
@var{port}, default to the console.")
   (let ((port (or port (current-output-port))))
     (newline port)
     (display-scheme-music music port))
   music)

displayScheme =
#(define-scheme-function (port expr) ((output-port?) scheme?)
   (_i "Display the internal representation of @var{expr} to
@var{port}, default to the console.")
   (let ((port (or port (current-output-port))))
     (newline port)
     (display-scheme-music expr port))
   expr)


dropNote =
#(define-music-function (num music) (integer? ly:music?)
   (_i "Drop a note of any chords in @var{music}, in @var{num}
position from above.")
   (music-map (move-chord-note (- num) DOWN) music))


endSpanners =
#(define-music-function (music) (ly:music?)
   (_i "Terminate the next spanner prematurely after exactly one note
without the need of a specific end spanner.")
   (let* ((start-span-evs (filter (lambda (ev)
                                    (equal? (ly:music-property ev 'span-direction)
                                            START))
                                  (extract-typed-music music 'span-event)))
          (stop-span-evs
           (map (lambda (m)
                  (music-clone m 'span-direction STOP))
                start-span-evs))
          (end-ev-chord (make-music 'EventChord
                                    'elements stop-span-evs))
          (total (make-music 'SequentialMusic
                             'elements (list music
                                             end-ev-chord))))
     total))

eventChords =
#(define-music-function (music) (ly:music?)
   (_i "Compatibility function wrapping @code{EventChord} around
isolated rhythmic events occuring since version 2.15.28, after
expanding repeat chords @samp{q}.")
   (event-chord-wrap! music))

featherDurations=
#(define-music-function (scale argument) (scale? ly:music?)
   (_i "Adjust durations of music in @var{argument} by @var{scale}.")
   (let ((orig-duration (ly:music-length argument))
         (factor (scale->factor scale))
         (multiplier 1))

     (for-each
      (lambda (mus)
        (if (< 0 (ly:moment-main-denominator (ly:music-length mus)))
            (begin
              (ly:music-compress mus multiplier)
              (set! multiplier (* factor multiplier)))))
      (extract-named-music argument '(EventChord NoteEvent RestEvent SkipEvent)))
     (ly:music-compress
      argument
      (ly:moment-div orig-duration (ly:music-length argument)))))

finger =
#(define-event-function (finger) (index-or-markup?)
   (_i "Apply @var{finger} as a fingering indication.")

   (make-music
            'FingeringEvent
            (if (index? finger) 'digit 'text) finger))

fixed =
#(define-music-function (pitch music)
   (ly:pitch? ly:music?)
   (_i "Use the octave of @var{pitch} as the default octave for @var{music}.")
   (let ((octave-marks (1+ (ly:pitch-octave pitch))))
     (cond ((not (= 0 octave-marks))
            (ly:music-transpose music (ly:make-pitch octave-marks 0 0))
            ;;In order to leave unchanged the notes in any enclosed
            ;; \absolute or \fixed or \relative, make a cancelling shift
            (map (lambda (m)
                   (ly:music-transpose m (ly:make-pitch (- octave-marks) 0 0)))
                 (extract-named-music music 'RelativeOctaveMusic)))))
   (make-music 'RelativeOctaveMusic 'element music))

footnote =
#(define-music-function (mark offset footnote item)
   ((markup?) number-pair? markup? symbol-list-or-music?)
   (_i "Make the markup @var{footnote} a footnote on @var{item}.  The
footnote is marked with a markup @var{mark} moved by @var{offset} with
respect to the marked music.

If @var{mark} is not given or specified as @var{\\default}, it is
replaced by an automatically generated sequence number.  If @var{item}
is a symbol list of form @samp{Grob} or @samp{Context.Grob}, then
grobs of that type will be marked at the current time step in the
given context (default @code{Bottom}).

If @var{item} is music, the music will get a footnote attached to a
grob immediately attached to the event, like @var{\\tweak} does.  For
attaching a footnote to an @emph{indirectly} caused grob, write
@code{\\single\\footnote}, use @var{item} to specify the grob, and
follow it with the music to annotate.

Like with @code{\\tweak}, if you use a footnote on a following
post-event, the @code{\\footnote} command itself needs to be attached
to the preceding note or rest as a post-event with @code{-}.")
   (let ((mus (make-music
               'FootnoteEvent
               'X-offset (car offset)
               'Y-offset (cdr offset)
               'automatically-numbered (not mark)
               'text (or mark (make-null-markup))
               'footnote-text footnote)))
     (once (propertyTweak 'footnote-music mus item))))

grace =
#(def-grace-function startGraceMusic stopGraceMusic
   (_i "Insert @var{music} as grace notes."))

grobdescriptions =
#(define-scheme-function (descriptions) (list?)
   (_i "Create a context modification from @var{descriptions}, a list
in the format of @code{all-grob-descriptions}.")
   (ly:make-context-mod
    (map (lambda (p)
           (list 'assign (car p) (ly:make-grob-properties (cdr p))))
         descriptions)))

harmonicByFret = #(define-music-function (fret music) (number? ly:music?)
  (_i "Convert @var{music} into mixed harmonics; the resulting notes resemble
harmonics played on a fretted instrument by touching the strings at @var{fret}.")
  #{
    \set harmonicDots = ##t
    \temporary \override TabNoteHead.stencil = #(tab-note-head::print-custom-fret-label (number->string fret))
    \temporary \override NoteHead.Y-extent = #grob::always-Y-extent-from-stencil
    \temporary \override NoteHead.stencil = #(lambda (grob) (ly:grob-set-property! grob 'style 'harmonic-mixed)
                                            (ly:note-head::print grob))
    #(make-harmonic
       (calc-harmonic-pitch (fret->pitch (number->string fret)) music))
    \unset harmonicDots
    \revert TabNoteHead.stencil
    \revert NoteHead.Y-extent
    \revert NoteHead.stencil
  #})

harmonicByRatio = #(define-music-function (ratio music) (number? ly:music?)
    (_i "Convert @var{music} into mixed harmonics; the resulting notes resemble
harmonics played on a fretted instrument by touching the strings at the point
given through @var{ratio}.")
  #{
    \set harmonicDots = ##t
    \temporary \override TabNoteHead.stencil = #(tab-note-head::print-custom-fret-label (ratio->fret ratio))
    \temporary \override NoteHead.Y-extent = #(ly:make-unpure-pure-container ly:grob::stencil-height)
    \temporary \override NoteHead.stencil = #(lambda (grob) (ly:grob-set-property! grob 'style 'harmonic-mixed)
                                            (ly:note-head::print grob))
    #(make-harmonic
      (calc-harmonic-pitch (ratio->pitch ratio) music))
    \unset harmonicDots
    \revert TabNoteHead.stencil
    \revert NoteHead.Y-extent
    \revert NoteHead.stencil
  #})

hide =
#(define-music-function (item) (symbol-list-or-music?)
   (_i "Set @var{item}'s @samp{transparent} property to @code{#t},
making it invisible while still retaining its dimensions.

If @var{item} is a symbol list of form @code{GrobName} or
@code{Context.GrobName}, the result is an override for the grob name
specified by it.  If @var{item} is a music expression, the result is
the same music expression with an appropriate tweak applied to it.")
   (propertyTweak 'transparent #t item))

inStaffSegno =
#(define-music-function () ()
   (_i "Put the segno variant 'varsegno' at this position into the staff,
compatible with the repeat command.")
   #{
     {
       \once \set Timing.segnoStyle = #'bar-line
       \segnoMark \default
     }
   #})

instrumentSwitch =
#(define-music-function
   (name) (string?)
   (_i "Switch instrument to @var{name}, which must have been predefined with
function @code{\\addInstrumentDefinition}.

This function is deprecated.")
   (let* ((handle (assoc name instrument-definitions))
          (instrument-def (if handle (cdr handle) '())))

     (if (not handle)
         (ly:input-warning (*location*) "No such instrument: ~a" name))
     (context-spec-music
      (make-music 'SimultaneousMusic
                  'elements
                  (map (lambda (kv)
                         (make-property-set
                          (car kv)
                          (cdr kv)))
                       instrument-def))
      'Staff)))

inversion =
#(define-music-function
   (around to music) (ly:pitch? ly:pitch? ly:music?)
   (_i "Invert @var{music} about @var{around} and
transpose from @var{around} to @var{to}.")
   (music-invert around to music))

invertChords =
#(define-music-function (num music) (integer? ly:music?)
   (_i "Invert any chords in @var{music} into their @var{num}-th position.
(Chord inversions may be directed downwards using negative integers.)")
   (let loop ((num num) (music music))
     (cond ((zero? num) music)
       ((negative? num) (loop (1+ num) (dropNote 1 music)))
       (else (loop (1- num) (raiseNote 1 music))))))


jump =
#(define-music-function (text) (markup?)
   "Mark a point of departure, e.g. @q{Gavotte I D.C.}."
   (make-music 'AdHocJumpEvent 'text text))



keepWithTag =
#(define-music-function (tags music)
   (symbol-list-or-symbol? ly:music?)
   (_i "Include only elements of @var{music} that are tagged with one
of the tags in @var{tags}.  @var{tags} may be either a single symbol
or a list of symbols.

Each tag may be declared as a member of at most one tag group (defined
with @code{\\tagGroup}).  If none of a @var{music} element's tags
share a tag group with one of the specified @var{tags}, the element is
retained.")
   (music-filter
    (tags-keep-predicate tags)
    music))

key =
#(define-music-function (tonic pitch-alist)
   ((ly:pitch? '()) (number-pair-list? '()))
   (_i "Set key to @var{tonic} and scale @var{pitch-alist}.
If both are null, just generate @code{KeyChangeEvent}.")
   (cond ((null? tonic) (make-music 'KeyChangeEvent))
         ((null? pitch-alist)
          (ly:parser-error (G_ "second argument must be pitch list")
                           (*location*))
          (make-music 'SequentialMusic 'void #t))
         (else
          (ly:music-transpose
           (make-music 'KeyChangeEvent
                'tonic (ly:make-pitch 0 0 0)
                'pitch-alist pitch-alist)
           tonic))))

killCues =
#(define-music-function (music) (ly:music?)
   (_i "Remove cue notes from @var{music}.")
   (music-map
    (lambda (mus)
      (if (and (string? (ly:music-property mus 'quoted-music-name))
               (string=? (ly:music-property mus 'quoted-context-id "") "cue"))
          (ly:music-property mus 'element)
          mus))
    music))



label =
#(define-music-function (label) (symbol?)
   (_i "Create @var{label} as a referrable label.")
   (make-music 'EventChord
               'page-marker #t
               'page-label label
               'elements (list (make-music 'LabelEvent
                                           'page-label label))))

previous-pitchnames = #'()

language =
#(define-void-function (language) (string?)
   (_i "Set note names for language @var{language}.")
   (note-names-language language))

languageSaveAndChange =
#(define-void-function (language) (string?)
  (_i "Store the previous pitchnames alist, and set a new one.")
  (set! previous-pitchnames pitchnames)
  (note-names-language language))

languageRestore =
#(define-void-function () ()
   (_i "Restore a previously-saved pitchnames alist.")
   (if previous-pitchnames
       (begin
        (set! pitchnames previous-pitchnames)
        (ly:parser-set-note-names pitchnames))
      (ly:input-warning (*location*) (G_ "No other language was defined previously. Ignoring."))))


magnifyMusic =
#(define-music-function (mag music) (positive? ly:music?)
   (_i "Magnify the notation of @var{music} without changing the
staff-size, using @var{mag} as a size factor.  Stems, beams,
slurs, ties, and horizontal spacing are adjusted automatically.")

   ;; these props are NOT allowed to shrink below default size
   (define unshrinkable-props
     '(
       ;; stems
       (Stem thickness)

       ;; slurs
       (Slur line-thickness)
       (Slur thickness)
       (PhrasingSlur line-thickness)
       (PhrasingSlur thickness)

       ;; ties
       (Tie line-thickness)
       (Tie thickness)
       (LaissezVibrerTie line-thickness)
       (LaissezVibrerTie thickness)
       (RepeatTie line-thickness)
       (RepeatTie thickness)
       ))

   ;; these props ARE allowed to shrink below default size
   (define shrinkable-props
     (let ((baseline-skip-props
             (find-named-props 'baseline-skip all-grob-descriptions))
           (word-space-props
             (find-named-props 'word-space all-grob-descriptions)))
       (append
         baseline-skip-props
         word-space-props
         '(
           ;; TODO: uncomment spacing-increment here once Issue 3987 is fixed
           ;; override at the 'Score level
           ;(SpacingSpanner spacing-increment)

           ;; lengths and heights
           (Beam length-fraction)
           (Stem length-fraction)
           (Stem beamlet-default-length)
           (Stem double-stem-separation)
           (Slur height-limit)
           (Slur minimum-length)
           (PhrasingSlur height-limit)
           (PhrasingSlur minimum-length)

           ;; Beam.beam-thickness is dealt with separately below
           ))))
   #{
     \context Bottom {
       %% TODO: uncomment \newSpacingSection once Issue 3990 is fixed
       %\newSpacingSection
       #(scale-fontSize 'magnifyMusic mag)
       #(scale-props    'magnifyMusic mag #f unshrinkable-props)
       #(scale-props    'magnifyMusic mag #t shrinkable-props)
       #(scale-beam-thickness mag)

       #music

       %% TODO: uncomment \newSpacingSection once Issue 3990 is fixed
       %\newSpacingSection
       %% reverse engineer the former fontSize value instead of using \unset
       #(revert-fontSize 'magnifyMusic mag)
       #(revert-props    'magnifyMusic mag (append unshrinkable-props
                                                   shrinkable-props
                                                   '((Beam beam-thickness))))
     }
   #})

magnifyStaff =
#(define-music-function (mag) (positive?)
   (_i "Change the size of the staff, adjusting notation size and
horizontal spacing automatically, using @var{mag} as a size factor.")

   ;; these props are NOT allowed to shrink below default size
   (define unshrinkable-props
     '((StaffSymbol thickness)))

   ;; these props ARE allowed to shrink below default size
   (define shrinkable-props
     (let* ((baseline-skip-props
              (find-named-props 'baseline-skip all-grob-descriptions))
            (word-space-props
              (find-named-props 'word-space all-grob-descriptions))
            (space-alist-props
              (find-named-props 'space-alist all-grob-descriptions)))
       (append
         baseline-skip-props
         word-space-props
         space-alist-props
         '(
           ;; override at the 'Score level
           (SpacingSpanner spacing-increment)

           (StaffSymbol staff-space)
           (BarLine kern)
           (BarLine segno-kern)
           (BarLine hair-thickness)
           (BarLine thick-thickness)
           (Stem beamlet-default-length)
           (Stem double-stem-separation)
           ))))

   #{
     \stopStaff

     %% revert settings from last time
     %% (but only if \magnifyStaff has already been used
     %% and the staff magnification is changing)
     #(revert-fontSize 'magnifyStaff mag)
     #(revert-props    'magnifyStaff mag (append unshrinkable-props
                                                 shrinkable-props))

     %% scale settings
     %% (but only if staff magnification is changing
     %% and does not equal 1)
     #(scale-fontSize 'magnifyStaff mag)
     #(scale-props    'magnifyStaff mag #f unshrinkable-props)
     #(scale-props    'magnifyStaff mag #t shrinkable-props)

     %% this might cause problems until Issue 3990 is fixed
     \newSpacingSection

     \startStaff
     \set Staff.magnifyStaffValue = #mag
   #})

makeClusters =
#(define-music-function (arg) (ly:music?)
   (_i "Display chords in @var{arg} as clusters.")
   (music-map note-to-cluster arg))

mark =
#(define-music-function (label) ((index-or-markup?))
   (_i "Create a rehearsal mark.  If @var{label} is an integer, create the
rehearsal mark for the given sequence number.  If @var{label} is
@code{\\default}, create the next sequential rehearsal mark.  If
@var{label} is markup, use it for the mark.")
   (cond ((not label)
          (make-music 'RehearsalMarkEvent))
         ((index? label)
          (make-music 'RehearsalMarkEvent 'label label))
         (else
          (make-music 'AdHocMarkEvent 'text label))))

markupMap =
#(define-music-function (path markupfun music)
   (symbol-list-or-symbol? markup-function? ly:music?)
   (_i "This applies the given markup function @var{markupfun} to all markup
music properties matching @var{path} in @var{music}.

For example,
@example
\\new Voice @{ g'2 c'' @}
\\addlyrics @{
  \\markupMap LyricEvent.text
             \\markup \\with-color #red \\etc
             @{ Oh yes! @}
@}
@end example
")
   (let* ((p (check-music-path path))
          (name (and p (car p)))
          (prop (and p (cadr p))))
     (if p
         (for-some-music
          (lambda (m)
            (if (or (not name) (eq? (ly:music-property m 'name) name))
                (let ((text (ly:music-property m prop)))
                  (if (markup? text)
                      (set! (ly:music-property m prop)
                            (list markupfun text)))))
            #f)
          music)))
   music)

modalInversion =
#(define-music-function (around to scale music)
    (ly:pitch? ly:pitch? ly:music? ly:music?)
    (_i "Invert @var{music} about @var{around} using @var{scale} and
transpose from @var{around} to @var{to}.")
    (let ((inverter (make-modal-inverter around to scale)))
      (change-pitches music inverter)
      music))

modalTranspose =
#(define-music-function (from to scale music)
    (ly:pitch? ly:pitch? ly:music? ly:music?)
    (_i "Transpose @var{music} from pitch @var{from} to pitch @var{to}
using @var{scale}.")
    (let ((transposer (make-modal-transposer from to scale)))
      (change-pitches music transposer)
      music))

musicMap =
#(define-music-function (proc mus) (procedure? ly:music?)
   (_i "Apply @var{proc} to @var{mus} and all of the music it contains.")
   (music-map proc mus))

%% noPageBreak and noPageTurn are music functions (not music identifiers),
%% because music identifiers are not allowed at top-level.
noPageBreak =
#(define-music-function () ()
   (_i "Forbid a page break.  May be used at toplevel (i.e., between scores or
markups), or inside a score.")
   (make-music 'EventChord
               'page-marker #t
               'page-break-permission 'forbid
               'elements (list (make-music 'PageBreakEvent
                                           'break-permission '()))))

noPageTurn =
#(define-music-function () ()
   (_i "Forbid a page turn.  May be used at toplevel (i.e., between scores or
markups), or inside a score.")
   (make-music 'EventChord
               'page-marker #t
               'page-turn-permission 'forbid
               'elements (list (make-music 'PageTurnEvent
                                           'break-permission '()))))



octaveCheck =
#(define-music-function (pitch) (ly:pitch?)
   (_i "Octave check.")
   (make-music 'RelativeOctaveCheck
               'pitch pitch))

offset =
#(define-music-function (property offsets item)
  (symbol-list-or-symbol? scheme? key-list-or-music?)
   (_i "Offset the default value of @var{property} of @var{item} by
@var{offsets}.  If @var{item} is a string, the result is
@code{\\override} for the specified grob type.  If @var{item} is
a music expression, the result is the same music expression with an
appropriate tweak applied.")
  (if (ly:music? item)
      ; In case of a tweak, grob property path is Grob.property
      (let ((prop-path (check-grob-path
                         (if (symbol? property)
                             (list property)
                             property)
                         #:start 1 #:default #t #:min 2 #:max 2)))
        (if prop-path
            ; If the head of the grob property path is a symbol--i.e.,
            ; a grob name, produce a directed tweak.  Otherwise, create
            ; an ordinary tweak.
            (if (symbol? (car prop-path))
                (tweak prop-path (offsetter (second prop-path) offsets) item)
                (tweak (second prop-path)
                       (offsetter (second prop-path) offsets)
                       item))
            item))
      ; In case of an override, grob property path is Context.Grob.property.
      (let ((prop-path (check-grob-path
                         (append item
                                 (if (symbol? property)
                                     (list property)
                                     property))
                         #:default 'Bottom #:min 3 #:max 3)))
        (if prop-path
            (propertyOverride prop-path (offsetter (third prop-path) offsets))
            (make-music 'Music)))))

omit =
#(define-music-function (item) (symbol-list-or-music?)
   (_i "Set @var{item}'s @samp{stencil} property to @code{#f},
effectively omitting it without taking up space.

If @var{item} is a symbol list of form @code{GrobName} or
@code{Context.GrobName}, the result is an override for the grob name
specified by it.  If @var{item} is a music expression, the result is
the same music expression with an appropriate tweak applied to it.")
   (propertyTweak 'stencil #f item))

once =
#(define-music-function (music) (ly:music?)
   (_i "Set @code{once} to @code{#t} on all layout instruction events
in @var{music}.  This will complain about music with an actual
duration.  As a special exception, if @var{music} might be the result
of a @code{\\tweak} command, no warning will be given in order to
allow for @code{\\once \\propertyTweak} to work as both one-time
override and proper tweak.")
   ;; is the warning worth this effort and reliable enough?
   ;;
   ;; Note that a repeat chord has a duration but an empty list of
   ;; tweakable music, so quiet? is trivially true for it.  Which is
   ;; just as well, considering that tweaks will not register on it.
   (let ((quiet? (every
                  (lambda (m) (pair? (ly:music-property m 'tweaks)))
                  (get-tweakable-music music))))
     (for-some-music
      (lambda (m)
        (cond ((music-is-of-type? m 'layout-instruction-event)
               (set! (ly:music-property m 'once) #t)
               #t)
              ((ly:duration? (ly:music-property m 'duration))
               (if (not quiet?)
                   (ly:music-warning m (G_ "Cannot apply \\once to timed music")))
               #t)
              (else #f)))
      music))
   music)

ottava =
#(define-music-function (octave) (integer?)
   (_i "Set the octavation.")
   (make-music 'OttavaEvent
               'ottava-number octave))

overrideTimeSignatureSettings =
#(define-music-function
   (time-signature base-moment beat-structure beam-exceptions)
   (fraction? fraction? list? list?)

   (_i "Override @code{timeSignatureSettings}
for time signatures of @var{time-signature} to have settings
of @var{base-moment}, @var{beat-structure}, and @var{beam-exceptions}.")

   ;; TODO -- add warning if largest value of grouping is
   ;;       greater than time-signature.
  (let ((setting (make-setting base-moment beat-structure beam-exceptions)))
    (override-time-signature-setting time-signature setting)))

overrideProperty =
#(define-music-function (grob-property-path value)
   (key-list? scheme?)

   (_i "Set the grob property specified by @var{grob-property-path} to
@var{value}.  @var{grob-property-path} is a symbol list of the form
@code{Context.GrobName.property} or @code{GrobName.property}, possibly
with subproperties given as well.

As opposed to @code{\\override} which overrides the context-dependent
defaults with which a grob is created, this command uses
@code{Output_property_engraver} at the grob acknowledge stage.  This
may be necessary for overriding values set after the initial grob
creation.")
   (let ((p (check-grob-path grob-property-path
                             #:default 'Bottom
                             #:min 3)))
     (if p
         (make-music 'ApplyOutputEvent
                     'context-type (first p)
                     'symbol (second p)
                     'procedure
                     (lambda (grob orig-context context)
                       (ly:grob-set-nested-property! grob (cddr p) value)))
         (make-music 'Music))))






%% pageBreak and pageTurn are music functions (iso music identifiers),
%% because music identifiers are not allowed at top-level.
pageBreak =
#(define-music-function () ()
   (_i "Force a page break.  May be used at toplevel (i.e., between scores or
markups), or inside a score.")
   (make-music 'EventChord
               'page-marker #t
               'line-break-permission 'force
               'page-break-permission 'force
               'elements (list (make-music 'LineBreakEvent
                                           'break-permission 'force)
                               (make-music 'PageBreakEvent
                                           'break-permission 'force))))

pageTurn =
#(define-music-function () ()
   (_i "Force a page turn between two scores or top-level markups.")
   (make-music 'EventChord
               'page-marker #t
               'line-break-permission 'force
               'page-break-permission 'force
               'page-turn-permission 'force
               'elements (list (make-music 'LineBreakEvent
                                           'break-permission 'force)
                               (make-music 'PageBreakEvent
                                           'break-permission 'force)
                               (make-music 'PageTurnEvent
                                           'break-permission 'force))))

parallelMusic =
#(define-void-function (voice-ids music) (list? ly:music?)
   (_i "Define parallel music sequences, separated by '|' (bar check signs),
and assign them to the identifiers provided in @var{voice-ids}.

@var{voice-ids}: a list of music identifiers (symbols containing only letters)

@var{music}: a music sequence, containing BarChecks as limiting expressions.

Example:

@verbatim
  \\parallelMusic A,B,C {
    c c | d d | e e |
    d d | e e | f f |
  }
<==>
  A = { c c | d d }
  B = { d d | e e }
  C = { e e | f f }
@end verbatim

The last bar checks in a sequence are not copied to the result in
order to facilitate ending the last entry at non-bar boundaries.
")
   (define voice-count (length voice-ids))
   (define (bar-check? m)
     "Checks whether m is a bar check."
     (eq? (ly:music-property m 'name) 'BarCheck))
   (define (recurse-and-split-list lst)
     "Return either a list of music lists split along barchecks, or @code{#f}."
     (if (any bar-check? lst)
         (let* ((voices (apply circular-list (make-list voice-count '())))
                (current-voices voices)
                (current-sequence '()))
           ;;
           ;; utilities
           (define (push-music m)
             "Push the music expression into the current sequence"
             (set! current-sequence (cons m current-sequence)))
           (define (change-voice)
             "Store the previously built sequence into the current voice and
change to the following voice."
             (set-car! current-voices
                       (cons current-sequence
                             (car current-voices)))
             (set! current-sequence '())
             (set! current-voices (cdr current-voices)))
           (for-each (lambda (m)
                       (let ((split? (recurse-and-split m)))
                         (if split?
                             (for-each
                              (lambda (m)
                                (push-music m)
                                (change-voice))
                              split?)
                             (begin
                               (push-music m)
                               (if (bar-check? m) (change-voice))))))
                     lst)
           (if (pair? current-sequence) (change-voice))
           ;; Un-circularize voices
           (set! voices (list-head voices voice-count))

           ;; Remove trailing bar checks to facilitate ending a
           ;; sequence on a non-bar, reverse partial sequences and sequences
           (set! voices (map!
                         (lambda (l)
                           (map! reverse!
                                 (reverse!
                                  (if (and (pair? l) (pair? (car l))
                                           (bar-check? (caar l)))
                                      (cons (cdar l) (cdr l))
                                      l))))
                         voices))

           ;; check sequence length
           (apply for-each (lambda seqs
                             (define (seq-len seq)
                               (reduce ly:moment-add
                                       (ly:make-moment 0)
                                       (map ly:music-length seq)))
                             (let ((moment-reference (seq-len (car seqs))))
                               (for-each (lambda (seq)
                                           (if (not (equal? (seq-len seq)
                                                            moment-reference))
                                               (ly:music-warning
                                                (if (pair? seq)
                                                    (last seq)
                                                    (caar seqs))
                                                (G_ "Bars in parallel music don't have the same length"))))
                                         seqs)))
                  voices)
           (map concatenate! voices))
         (let ((deeper (map recurse-and-split lst)))
           (and (any pair? deeper)
                (apply zip (map
                            (lambda (m split)
                              (or split
                                  (ly:music-deep-copy (make-list voice-count m))))
                            lst deeper))))))
   (define (recurse-and-split music)
     "This returns either a list of music split along barchecks, or
@code{#f}."
     (let* ((elt (ly:music-property music 'element))
            (elts (ly:music-property music 'elements))
            (split-elt (and (ly:music? elt) (recurse-and-split elt)))
            (split-elts (and (pair? elts) (recurse-and-split-list elts))))
       (and (or split-elt split-elts)
            (map
             (lambda (e es)
               (let ((m (ly:music-deep-copy music
                       ;;; reassigning the origin of the parent only
                       ;;; makes sense if the first expression in the
                       ;;; result is from a distributed origin
                                            (or (and (ly:music? e) e)
                                                (and (pair? es) (car es))))))
                 (if (ly:music? e)
                     (set! (ly:music-property m 'element) e))
                 (if (pair? es)
                     (set! (ly:music-property m 'elements) es))
                 m))
             (or split-elt (circular-list #f))
             (or split-elts (circular-list #f))))))
   (let ((voices (recurse-and-split music)))
     (if voices
         ;;
         ;; bind voice identifiers to the voices
         (for-each (lambda (voice-id voice)
                     (ly:parser-define! voice-id voice))
                   voice-ids voices)
         (ly:music-warning music
                           (G_ "ignoring parallel music without barchecks")))))

parenthesize =
#(define-music-function (arg) (symbol-list-or-music?)
   (_i "Tag @var{arg} to be parenthesized.  @var{arg} may
be either a music event or a grob path.")
   (let ((maybe-with-id (if (and (ly:music? arg)
                                 (music-is-of-type? arg 'event-chord))
                            (tweak 'parenthesis-id
                                   (gensym "unique-parenthesis-id")
                                   arg)
                            arg)))
     (once (propertyTweak 'parenthesized #t maybe-with-id))))

#(define (make-directed-part-combine-music direction chord-range part1 part2
          one-context-settings
          two-context-settings
          shared-context-settings)

   (let* ((pc-music (make-music 'PartCombineMusic))
          (listener (ly:parser-lookup 'partCombineListener))
          (evs2 (recording-group-emulate
                 (context-spec-music (make-non-relative-music part2)
                                     'Voice "two")
                 listener))
          (evs1 (recording-group-emulate
                 (context-spec-music (make-non-relative-music part1)
                                     'Voice "one")
                 listener))
          (split-list
           (determine-split-list (reverse! (assoc-get "one" evs1 '()) '())
                                 (reverse! (assoc-get "two" evs2 '()) '())
                                 chord-range))
          (L1 (ly:music-length part1))
          (L2 (ly:music-length part2))
          ;; keep the contexts alive for the full duration
          (skip (make-skip-music (make-duration-of-length
                                  (if (ly:moment<? L1 L2) L2 L1)))))

     ;; \partCombine depends on Simultaneous_music_iterator's
     ;; processing its children in order (at each timestep) so that
     ;; context changes occur before notation events.  If
     ;; Simultaneous_music_iterator's behavior is ever changed, this
     ;; will need to be changed too.
     (define (make-part-music initial-voice-name changes notation)
       (context-spec-music
        ;; TODO: Is non-relative music necessary here, or only for the
        ;; analysis above?
        (make-music 'SimultaneousMusic
                    'elements (list changes (make-non-relative-music notation))
                    'tags '($partCombine))
        'Voice initial-voice-name))

     (set! (ly:music-property pc-music 'elements)
           (list (make-part-music
                  "one"
                  (make-part-combine-context-changes
                   default-part-combine-context-change-state-machine-one
                   split-list)
                  part1)
                 (make-part-music
                  "two"
                  (make-part-combine-context-changes
                   default-part-combine-context-change-state-machine-two
                   split-list)
                  part2)))

     (set! (ly:music-property pc-music 'direction) direction)

     #{ \context Staff <<
          \context Voice = "one" \with #one-context-settings { #skip }
          \context Voice = "two" \with #two-context-settings { #skip }
          \context Voice = "shared" \with #shared-context-settings { #skip }
          \context Voice = "solo" { #skip }
          \context NullVoice = "null" { #skip }
          #pc-music
          #(make-part-combine-marks
            default-part-combine-mark-state-machine split-list)
        >> #} ))

partCombine =
#(define-music-function (chord-range part1 part2)
   ((number-pair? '(0 . 8)) ly:music? ly:music?)
   (_i "Take the music in @var{part1} and @var{part2} and return
a music expression containing simultaneous voices, where @var{part1}
and @var{part2} are combined into one voice where appropriate.
Optional @var{chord-range} sets the distance in steps between notes
that may be combined into a chord or unison.")
   (make-directed-part-combine-music #f chord-range part1 part2
    #{ \with { \voiceOne \override DynamicLineSpanner.direction = #UP } #}
    #{ \with { \voiceTwo \override DynamicLineSpanner.direction = #DOWN } #}
    #{ #} ))

partCombineUp =
#(define-music-function (chord-range part1 part2)
   ((number-pair? '(0 . 8)) ly:music? ly:music?)
   (_i "Take the music in @var{part1} and @var{part2} and typeset so
that they share a staff with stems directed upward.")
   (make-directed-part-combine-music UP chord-range part1 part2
    #{ \with { \voiceOne \override DynamicLineSpanner.direction = #UP } #}
    #{ \with { \voiceThree \override DynamicLineSpanner.direction = #UP } #}
    #{ \with { \voiceOne \override DynamicLineSpanner.direction = #UP } #} ))

partCombineDown =
#(define-music-function (chord-range part1 part2)
   ((number-pair? '(0 . 8)) ly:music? ly:music?)
   (_i "Take the music in @var{part1} and @var{part2} and typeset so
that they share a staff with stems directed downward.")
   (make-directed-part-combine-music DOWN chord-range part1 part2
    #{ \with { \voiceFour \override DynamicLineSpanner.direction = #DOWN } #}
    #{ \with { \voiceTwo \override DynamicLineSpanner.direction = #DOWN } #}
    #{ \with { \voiceTwo \override DynamicLineSpanner.direction = #DOWN } #} ))

%% Part combine forcing to be found in ly/property-init.ly

partial =
#(define-music-function (dur) (ly:duration?)
  (_i "Make a partial measure.")
  (context-spec-music (make-music 'PartialSet
                                  'origin (*location*)
                                  'duration dur)
                      'Timing))

pitchedTrill =
#(define-music-function
   (main-note secondary-note)
   (ly:music? ly:music?)
   (_i "Print a trill with @var{main-note} as the main note of the trill and
print @var{secondary-note} as a stemless note head in parentheses.")
   (let* ((get-notes (lambda (ev-chord)
                       (extract-named-music ev-chord 'NoteEvent)))
          (sec-note-events (get-notes secondary-note))
          (trill-events (extract-named-music main-note 'TrillSpanEvent)))
     (if (pair? sec-note-events)
         (begin
           (let* ((trill-pitch (ly:music-property (car sec-note-events) 'pitch))
                  (forced (ly:music-property (car sec-note-events) 'force-accidental)))

             (if (ly:pitch? trill-pitch)
                 (for-each (lambda (m)
                             (ly:music-set-property! m 'pitch trill-pitch)) trill-events)
                 (begin
                   (ly:input-warning (*location*) (G_ "Second argument of \\pitchedTrill should be single note: "))
                   (display sec-note-events)))

             (if (eq? forced #t)
                 (for-each (lambda (m)
                             (ly:music-set-property! m 'force-accidental forced))
                           trill-events)))))
     main-note))

propertyOverride =
#(define-music-function (grob-property-path value)
   (key-list? scheme?)
   (_i "Set the grob property specified by @var{grob-property-path} to
@var{value}.  @var{grob-property-path} is a symbol list of the form
@code{Context.GrobName.property} or @code{GrobName.property}, possibly
with subproperties given as well.  This music function is mostly intended
for use from Scheme as a substitute for the built-in @code{\\override}
command.")
   (let ((p (check-grob-path grob-property-path
                             #:default 'Bottom
                             #:min 3)))
     (if p
         (context-spec-music
          (make-music 'OverrideProperty
                      'symbol (cadr p)
                      'origin (*location*)
                      'grob-value value
                      'grob-property-path (cddr p)
                      'pop-first #t)
          (car p))
         (make-music 'Music))))

propertyRevert =
#(define-music-function (grob-property-path)
   (key-list?)
   (_i "Revert the grob property specified by @var{grob-property-path} to
its previous value.  @var{grob-property-path} is a symbol list of the form
@code{Context.GrobName.property} or @code{GrobName.property}, possibly
with subproperties given as well.  This music function is mostly intended
for use from Scheme as a substitute for the built-in @code{\\revert}
command.")
   (let ((p (check-grob-path grob-property-path
                             #:default 'Bottom
                             #:min 3)))
     (if p
         (context-spec-music
          (make-music 'RevertProperty
                      'symbol (cadr p)
                      'origin (*location*)
                      'grob-property-path (cddr p))
          (car p))
         (make-music 'Music))))

propertySet =
#(define-music-function (property-path value)
   (symbol-list-or-symbol? scheme?)
   (_i "Set the context property specified by @var{property-path} to
@var{value}.  This music function is mostly intended for use from
Scheme as a substitute for the built-in @code{\\set} command.")
   (let ((p (check-context-path property-path)))
     (if p
         (context-spec-music
          (make-music 'PropertySet
                      'symbol (cadr p)
                      'value value
                      'origin (*location*))
          (car p))
         (make-music 'Music))))

propertyTweak =
#(define-music-function (prop value item)
   (key-list-or-symbol? scheme? key-list-or-music?)
   (_i "Add a tweak to the following @var{item}, usually music.
This generally behaves like @code{\\tweak} but will turn into an
@code{\\override} when @var{item} is a symbol list.

In that case, @var{item} specifies the grob path to override.  This is
mainly useful when using @code{\\propertyTweak} as as a component for
building other functions like @code{\\omit}.  It is not the default
behavior for @code{\\tweak} since many input strings in
@code{\\lyricmode} can serve equally as music or as symbols which
causes surprising behavior when tweaking lyrics using the less
specific semantics of @code{\\propertyTweak}.

@var{prop} can contain additional elements in which case a nested
property (inside of an alist) is tweaked.")
   ;; Why not instead make the parser treat strings preferably as
   ;; music in lyrics mode rather than as symbol?  Because then
   ;;
   ;; \tweak text "whatever" mylyrics
   ;;
   ;; will try putting a lyric event with text "whatever" in the text
   ;; property of lyrics.  So we want expressions allowing both
   ;; strings and lyrics to deliver strings: more complex conversions
   ;; should only be attempted when the simple uses don't match the
   ;; given predicate.
   (if (ly:music? item)
       (if (music-is-of-type? item 'context-specification)
           ;; This is essentially dealing with the case
           ;; \propertyTweak color #red \propertyTweak font-size #3 NoteHead
           ;; namely when stacked tweaks end in a symbol list
           ;; rather than a music expression.
           ;;
           ;; We have a tweak here to convert into an override,
           ;; so we need to know the grob to apply it to.  That's
           ;; easy if we have a directed tweak, and otherwise we
           ;; need to find the symbol in the expression itself.
           (let* ((p (check-grob-path prop
                                      #:start 1
                                      #:default #t
                                      #:min 2))
                  (elt (ly:music-property item 'element))
                  (seq (if (music-is-of-type? elt 'sequential-music)
                           elt
                           (make-sequential-music (list elt))))
                  (elts (ly:music-property seq 'elements))
                  (symbol (if (symbol? (car p))
                              (car p)
                              (and (pair? elts)
                                   (ly:music-property (car elts)
                                                      'symbol)))))
             (if (symbol? symbol)
                 (begin
                   (set! (ly:music-property seq 'elements)
                         (cons (make-music 'OverrideProperty
                                           'symbol symbol
                                           'grob-property-path (cdr p)
                                           'pop-first #t
                                           'grob-value value
                                           'origin (*location*))
                               elts))
                   (set! (ly:music-property item 'element) seq))
                 (begin
                   (ly:parser-error (G_ "Cannot \\propertyTweak")
                                    (*location*))
                   (ly:music-message item (G_ "untweakable"))))
             item)
           (tweak prop value item))
       (propertyOverride (append item (if (symbol? prop) (list prop) prop))
                         value)))

propertyUnset =
#(define-music-function (property-path)
   (symbol-list-or-symbol?)
   (_i "Unset the context property specified by @var{property-path}.
This music function is mostly intended for use from Scheme as a
substitute for the built-in @code{\\unset} command.")
   (let ((p (check-context-path property-path)))
     (if p
         (context-spec-music
          (make-music 'PropertyUnset
                      'symbol (cadr p)
                      'origin (*location*))
          (car p))
         (make-music 'Music))))

pushToTag =
#(define-music-function (tag more music)
   (symbol? ly:music? ly:music?)
   (_i "Add @var{more} to the front of music tagged with @var{tag}.  A
@code{post-event} can be added to the articulations of rhythmic events
or chords; other expressions may be added to chords, sequential or
simultaneous music.")
   (define (add-left m more)
     (cond
      ((or (music-is-of-type? m 'sequential-music)
           (music-is-of-type? m 'simultaneous-music))
       (if (ly:event? more)
           (begin
             (ly:music-warning more (G_ "\\pushToTag cannot push post-event"))
             (ly:music-message m (G_ "to this music")))
           (set! (ly:music-property m 'elements)
                 (cons more (ly:music-property m 'elements)))))
      ((music-is-of-type? m 'event-chord)
       (cond ((ly:event? more)
              (set! (ly:music-property m 'elements)
                    (call-with-values (lambda ()
                                        (break! ly:event?
                                                (ly:music-property m 'elements)))
                      (lambda (elts posts)
                        (append! elts (cons more posts))))))
             ((music-is-of-type? more 'rhythmic-event)
              (set! (ly:music-property m 'elements)
                    (cons more (ly:music-property m 'elements))))
             (else
              (ly:music-warning more (G_ "\\pushToTag cannot push this"))
              (ly:music-message m (G_ "to this event-chord")))))
      ((music-is-of-type? m 'rhythmic-event)
       (if (ly:event? more)
           (set! (ly:music-property m 'articulations)
                 (cons more (ly:music-property m 'articulations)))
           (begin
             (ly:music-warning more (G_ "\\pushToTag cannot push this music"))
             (ly:music-message m (G_ "to this rhythmic-event")))))
      ((music-is-of-type? m 'music-wrapper-music)
       (add-left (ly:music-property m 'element) more))
      (else
       (ly:input-warning (*location*) (G_ "\\pushToTag failed:"))
       (ly:music-message m (G_ "No \\pushToTag destination")))))
   (music-map (lambda (m)
                (if (memq tag (ly:music-property m 'tags))
                    (add-left m more))
                m)
              music))

quoteDuring =
#(define-music-function (what main-music) (string? ly:music?)
   (_i "Indicate a section of music to be quoted.  @var{what} indicates the name
of the quoted voice, as specified in an @code{\\addQuote} command.
@var{main-music} is used to indicate the length of music to be quoted;
usually contains spacers or multi-measure rests.")
   (make-music 'QuoteMusic
               'element main-music
               'quoted-music-name what))

raiseNote =
#(define-music-function (num music) (integer? ly:music?)
   (_i "Raise a note of any chords in @var{music}, in @var{num}
position from below.")
   (music-map (move-chord-note (1- num) UP) music))

reduceChords =
#(define-music-function (music) (ly:music?)
   (_i "Reduce chords contained in @var{music} to single notes,
intended mainly for reusing music in RhythmicStaff.  Does not
reduce parallel music.")
   (event-chord-reduce music))

relative =
#(define-music-function (pitch music)
   ((ly:pitch?) ly:music?)
   (_i "Make @var{music} relative to @var{pitch}.  If @var{pitch} is
omitted, the first note in @var{music} is given in absolute pitch.")
   ;; When \relative has no clear decision (can only happen with
   ;; scales with an even number of steps), it goes down (see
   ;; pitch.cc).  The following formula puts out f for both the normal
   ;; 7-step scale as well as for a "shortened" scale missing the
   ;; final b.  In either case, a first note of c will end up as c,
   ;; namely pitch (-1, 0, 0).
   (ly:make-music-relative! music
                            (or pitch
                                (ly:make-pitch
                                 -1
                                 (quotient
                                  ;; size of current scale:
                                  (ly:pitch-steps (ly:make-pitch 1 0))
                                  2))))
   (make-music 'RelativeOctaveMusic
               'element music))

removeWithTag =
#(define-music-function (tags music)
   (symbol-list-or-symbol? ly:music?)
   (_i "Remove elements of @var{music} that are tagged with one of the
tags in @var{tags}.  @var{tags} may be either a single symbol or a list
of symbols.")
   (music-filter
    (tags-remove-predicate tags)
    music))

resetRelativeOctave =
#(define-music-function (pitch) (ly:pitch?)
   (_i "Set the octave inside a \\relative section.")

   (make-music 'SequentialMusic
               'to-relative-callback
               (lambda (music last-pitch) pitch)))

retrograde =
#(define-music-function (music)
    (ly:music?)
    (_i "Return @var{music} in reverse order.")
    (retrograde-music
     (expand-repeat-notes!
      (expand-repeat-chords!
       (cons 'rhythmic-event
             (ly:parser-lookup '$chord-repeat-events))
       music))))

revertTimeSignatureSettings =
#(define-music-function
   (time-signature)
   (pair?)

   (_i "Revert @code{timeSignatureSettings}
for time signatures of @var{time-signature}.")
   (revert-time-signature-setting time-signature))

rightHandFinger =
#(define-event-function (finger) (index-or-markup?)
   (_i "Apply @var{finger} as a fingering indication.")

   (make-music
            'StrokeFingerEvent
            (if (index? finger) 'digit 'text) finger))

scaleDurations =
#(define-music-function (fraction music)
   (scale? ly:music?)
   (_i "Multiply the duration of events in @var{music} by @var{fraction}.")
   (ly:music-compress music fraction))

sectionLabel =
#(define-music-function (text) (markup?)
   (_i "Mark the beginning of a named passage, e.g. @qq{Coda}.  This
is well suited for use at a section division created with
@code{\\section}, but it does not imply @code{\\section} and may be
used alone.")
   (make-music 'SectionLabelEvent 'text text))

segnoMark =
#(define-music-function (num) ((index?))
   (_i "Create a segno mark (or bar line, if the @code{segnoStyle}
context property is @code{'bar-line}).  @var{num} may be 1@tie{}for
the first segno, 2@tie{}for the second, etc., or it may be
@code{\\default} to use the next number in sequence automatically.")
   (if num
       (make-music 'SegnoMarkEvent 'label num)
       (make-music 'SegnoMarkEvent)))

settingsFrom =
#(define-scheme-function (ctx music)
   ((symbol?) ly:music?)
   (_i "Take the layout instruction events from @var{music}, optionally
restricted to those applying to context type @var{ctx}, and return
a context modification duplicating their effect.")
   (let ((mods (ly:make-context-mod)))
     (define (musicop m)
       (if (music-is-of-type? m 'layout-instruction-event)
           (ly:add-context-mod
            mods
            (case (ly:music-property m 'name)
              ((PropertySet)
               (list 'assign
                     (ly:music-property m 'symbol)
                     (ly:music-property m 'value)))
              ((PropertyUnset)
               (list 'unset
                     (ly:music-property m 'symbol)))
              ((OverrideProperty)
               (cons* 'push
                      (ly:music-property m 'symbol)
                      (ly:music-property m 'grob-value)
                      (cond
                       ((ly:music-property m 'grob-property #f) => list)
                       (else
                        (ly:music-property m 'grob-property-path)))))
              ((RevertProperty)
               (cons* 'pop
                      (ly:music-property m 'symbol)
                      (cond
                       ((ly:music-property m 'grob-property #f) => list)
                       (else
                        (ly:music-property m 'grob-property-path)))))))
           (case (ly:music-property m 'name)
             ((ApplyContext)
              (ly:add-context-mod mods
                                  (list 'apply
                                        (ly:music-property m 'procedure))))
             ((ContextSpeccedMusic)
              (if (or (not ctx)
                      (eq? ctx (ly:music-property m 'context-type)))
                  (musicop (ly:music-property m 'element))))
             (else
              (let ((callback (ly:music-property m 'elements-callback)))
                (if (procedure? callback)
                    (for-each musicop (callback m))))))))
     (musicop music)
     mods))

shape =
#(define-music-function (offsets item)
   (list? key-list-or-music?)
   (_i "Offset control-points of @var{item} by @var{offsets}.  The
argument is a list of number pairs or list of such lists.  Each
element of a pair represents an offset to one of the coordinates of a
control-point.  The y-coordinate of each number pair is scaled by staff space.
If @var{item} is a string, the result is @code{\\once\\override} for the
specified grob type.  If @var{item} is a music expression, the result is the
same music expression with an appropriate tweak applied.")
   (define (shape-curve grob coords)
     (let* ((orig (ly:grob-original grob))
            (siblings (if (ly:spanner? grob)
                          (ly:spanner-broken-into orig) '()))
            (total-found (length siblings))
            (staff-space (ly:staff-symbol-staff-space grob))
            (scaled-offsets
              (map
                (lambda (offset)
                  (if (number-pair? offset)
                      (cons (car offset) (* (cdr offset) staff-space))
                      offset))
                offsets)))

       (define (offset-control-points offsets)
         (if (null? offsets)
             coords
             (map coord-translate coords offsets)))

       (define (helper sibs offs)
         (if (pair? offs)
             (if (eq? (car sibs) grob)
                 (offset-control-points (car offs))
                 (helper (cdr sibs) (cdr offs)))
             coords))

       ;; we work with lists of lists
       (if (or (null? scaled-offsets)
               (not (list? (car scaled-offsets))))
           (set! scaled-offsets (list scaled-offsets)))

       (if (>= total-found 2)
           (helper siblings scaled-offsets)
           (offset-control-points (car scaled-offsets)))))

   (once (propertyTweak 'control-points
                        (grob-transformer 'control-points shape-curve)
                        item)))

shiftDurations =
#(define-music-function (dur dots arg)
   (integer? integer? ly:music?)
   (_i "Change the duration of @var{arg} by adding @var{dur} to the
@code{durlog} of @var{arg} and @var{dots} to the @code{dots} of @var{arg}.")

   (shift-duration-log arg dur dots))

single =
#(define-music-function (overrides music)
   (ly:music? ly:music?)
   (_i "Convert @var{overrides} to tweaks and apply them to @var{music}.
This does not convert @code{\\revert}, @code{\\set} or @code{\\unset}.")
   (fold-some-music
    (lambda (m) (eq? (ly:music-property m 'name)
                     'OverrideProperty))
    (lambda (m music)
      (tweak (cons (ly:music-property m 'symbol) ;grob name
                   (cond
                    ((ly:music-property m 'grob-property #f) => list)
                    (else
                     (ly:music-property m 'grob-property-path))))
             (ly:music-property m 'grob-value) music))
    music
    overrides))

skip =
#(define-music-function (arg) (duration-or-music?)
  (_i "Skip over @var{arg}, which may be music or a duration.")
  (if (ly:duration? arg)
      (make-music 'SkipMusic
                  'duration arg)
      (make-music 'SkippedMusic
                  'element arg)))


slashedGrace =
#(def-grace-function startSlashedGraceMusic stopSlashedGraceMusic
   (_i "Create slashed graces (slashes through stems, but no slur) from
the following music expression"))

styledNoteHeads =
#(define-music-function (style heads music)
   (symbol? symbol-list-or-symbol? ly:music?)
   (_i "Set @var{heads} in @var{music} to @var{style}.")
   (style-note-heads heads style music))

tag =
#(define-music-function (tags music) (symbol-list-or-symbol? ly:music?)
   (_i "Tag the following @var{music} with @var{tags} and return the
result, by adding the single symbol or symbol list @var{tags} to the
@code{tags} property of @var{music}.")

   (set!
    (ly:music-property music 'tags)
    ((if (symbol? tags) cons append)
     tags
     (ly:music-property music 'tags)))
   music)

tagGroup =
#(define-void-function (tags) (symbol-list?)
   (_i "Define a tag group comprising the symbols in the symbol list
@var{tags}.  Tag groups must not overlap.")
   (let ((err (define-tag-group tags)))
     (if err (ly:parser-error err (*location*)))))

temporary =
#(define-music-function (music)
   (ly:music?)
   (_i "Make any @code{\\override} in @var{music} replace an existing
grob property value only temporarily, restoring the old value when a
corresponding @code{\\revert} is executed.  This is achieved by
clearing the @samp{pop-first} property normally set on
@code{\\override}s.

An @code{\\override}/@/@code{\\revert} sequence created by using
@code{\\temporary} and @code{\\undo} on the same music containing
overrides will cancel out perfectly or cause a@tie{}warning.

Non-property-related music is ignored, warnings are generated for any
property-changing music that isn't an @code{\\override}.")
   (define warned #f)
   (for-some-music
    (lambda (m)
      (and (or (music-is-of-type? m 'layout-instruction-event)
               (music-is-of-type? m 'context-specification)
               (music-is-of-type? m 'apply-context)
               (music-is-of-type? m 'time-signature-music))
           (case (ly:music-property m 'name)
             ((OverrideProperty)
              (if (ly:music-property m 'pop-first #f)
                  (set! (ly:music-property m 'pop-first) '()))
              (if (ly:music-property m 'once #f)
                  (set! (ly:music-property m 'once) '()))
              #t)
             ((ContextSpeccedMusic)
              #f)
             (else
              (if (not warned)
                  (begin
                    (ly:input-warning (*location*) (G_ "Cannot make ~a revertible")
                                      (ly:music-property m 'name))
                    (set! warned #t)))
              #t))))
    music)
   music)

%% The reasoning behind using RIGHT for \textMark and LEFT for \textEndMark
%% is that we may want to generalize horizontal-direction to be also a grob
%% property and apply to more items, and this way it is similar to the break
%% status direction of an item.

textMark =
#(define-music-function (text) (markup?)
   (make-music 'TextMarkEvent 'text text 'horizontal-direction RIGHT))

textEndMark =
#(define-music-function (text) (markup?)
   (make-music 'TextMarkEvent 'text text 'horizontal-direction LEFT))

time =
#(define-music-function (beat-structure fraction)
   ((number-list? '()) fraction?)
   (_i "Set @var{fraction} as time signature, with optional
number list @var{beat-structure} before it.")
  (make-music 'TimeSignatureMusic
              'numerator (car fraction)
              'denominator (cdr fraction)
              'beat-structure beat-structure))

times =
#(define-music-function (fraction music)
   (fraction? ly:music?)
   (_i "Scale @var{music} in time by @var{fraction}.")
  (make-music 'TimeScaledMusic
              'element (ly:music-compress music (ly:make-moment (car fraction) (cdr fraction)))
              'numerator (car fraction)
              'denominator (cdr fraction)))

transpose =
#(define-music-function
   (from to music)
   (ly:pitch? ly:pitch? ly:music?)

   (_i "Transpose @var{music} from pitch @var{from} to pitch @var{to}.")
   (make-music 'TransposedMusic
               'element (ly:music-transpose music (ly:pitch-diff to from))))

transposedCueDuring =
#(define-music-function
   (what dir pitch main-music)
   (string? ly:dir? ly:pitch? ly:music?)

   (_i "Insert notes from the part @var{what} into a voice called @code{cue},
using the transposition defined by @var{pitch}.  This happens
simultaneously with @var{main-music}, which is usually a rest.  The
argument @var{dir} determines whether the cue notes should be notated
as a first or second voice.")

   (make-music 'QuoteMusic
               'element main-music
               'quoted-context-type 'CueVoice
               'quoted-context-id "cue"
               'quoted-music-name what
               'quoted-voice-direction dir
               ;; following is inverse of instrumentTransposition for
               ;; historical reasons
               'quoted-transposition pitch))

transposition =
#(define-music-function (pitch) (ly:pitch?)
   (_i "Set instrument transposition")

   (context-spec-music
    (make-property-set 'instrumentTransposition pitch)
    'Staff))

tuplet =
#(define-music-function (ratio tuplet-span music)
   (fraction? (ly:duration? '()) ly:music?)
   (_i "Scale the given @var{music} to tuplets.  @var{ratio} is a
fraction that specifies how many notes are played in place of the
nominal value: it will be @samp{3/2} for triplets, namely three notes
being played in place of two.  If the optional duration
@var{tuplet-span} is specified, it is used instead of
@code{tupletSpannerDuration} for grouping the tuplets.
For example,
@example
\\tuplet 3/2 4 @{ c8 c c c c c @}
@end example
will result in two groups of three tuplets, each group lasting for a
quarter note.")
   (make-music 'TimeScaledMusic
               'element (ly:music-compress
                         music
                         (ly:make-moment (cdr ratio) (car ratio)))
               'numerator (cdr ratio)
               'denominator (car ratio)
               'duration tuplet-span))

tupletSpan =
#(define-music-function (tuplet-span)
   ((ly:duration?))
   (_i "Set @code{tupletSpannerDuration}, the length into which
@code{\\tuplet} without an explicit @samp{tuplet-span} argument of its
own will group its tuplets, to the duration @var{tuplet-span}.  To
revert to the default of not subdividing the contents of a @code{\\tuplet}
command without explicit @samp{tuplet-span}, use
@example
\\tupletSpan \\default
@end example
")
   (if tuplet-span
       #{ \set tupletSpannerDuration = #(ly:duration-length tuplet-span) #}
       #{ \unset tupletSpannerDuration #}))

tweak =
#(define-music-function (prop value music)
   (key-list-or-symbol? scheme? ly:music?)
   (_i "Add a tweak to the following @var{music}.
Layout objects created by @var{music} get their property @var{prop}
set to @var{value}.  If @var{prop} has the form @samp{Grob.property}, like with
@example
\\tweak Accidental.color #red cis'
@end example
an indirectly created grob (@samp{Accidental} is caused by
@samp{NoteHead}) can be tweaked; otherwise only directly created grobs
are affected.

@var{prop} can contain additional elements in which case a nested
property (inside of an alist) is tweaked.

If @var{music} is an @samp{event-chord}, every contained
@samp{rhythmic-event} is tweaked instead.")
   (let ((p (check-grob-path prop
                             #:start 1
                             #:default #t
                             #:min 2)))
     (define (tweak-this music)
       (set! (ly:music-property music 'tweaks)
             (acons (cond ((pair? (cddr p)) p)
                          ((symbol? (car p))
                           (cons (car p) (cadr p)))
                          (else (cadr p)))
                    value
                    (ly:music-property music 'tweaks))))
     (if p
         ;; p now contains at least two elements.  The first
         ;; element is #t when no grob has been explicitly
         ;; specified, otherwise it is a grob name.
         (for-each tweak-this (get-tweakable-music music)))
     music))


undo =
#(define-music-function (music)
   (ly:music?)
   (_i "Convert @code{\\override} and @code{\\set} in @var{music} to
@code{\\revert} and @code{\\unset}, respectively.  Any reverts and
unsets already in @var{music} cause a warning.  Non-property-related music is ignored.")
   (define warned #f)
   (let loop
       ((music music))
     (let
         ((lst
           (fold-some-music
            (music-type-predicate '(layout-instruction-event
                                    context-specification
                                    apply-context
                                    time-signature-music))
            (lambda (m overrides)
              (case (ly:music-property m 'name)
                ((OverrideProperty)
                 (cons
                  (make-music 'RevertProperty
                              'symbol (ly:music-property m 'symbol)
                              'grob-property-path
                              (cond
                               ((ly:music-property m 'grob-property #f) => list)
                               (else
                                (ly:music-property m 'grob-property-path))))
                  overrides))
                ((PropertySet)
                 (cons
                  (make-music 'PropertyUnset
                              'symbol (ly:music-property m 'symbol))
                  overrides))
                ((ContextSpeccedMusic)
                 (cons
                  (make-music 'ContextSpeccedMusic
                              'element (loop (ly:music-property m 'element))
                              'context-type (ly:music-property m 'context-type))
                  overrides))
                (else
                 (if (not warned)
                     (begin
                       (ly:input-warning (*location*) (G_ "Cannot revert ~a")
                                         (ly:music-property m 'name))
                       (set! warned #t)))
                 overrides)))
            '()
            music)))
       (cond
        ((null? lst) (make-music 'Music))
        ((null? (cdr lst)) (car lst))
        (else (make-sequential-music lst))))))

unfolded =
#(define-music-function (music)
   (ly:music?)
   (_i "Mask @var{music} until the innermost enclosing repeat is
unfolded.")
   (make-music 'UnfoldedSpeccedMusic
               'element music))

unfoldRepeats =
#(define-music-function (types music)
   ((symbol-list-or-symbol? '()) ly:music?)
   (_i "Force @code{\\repeat volta}, @code{\\repeat tremolo} or
@code{\\repeat percent} commands in @var{music} to be interpreted
as @code{\\repeat unfold}, if specified in the optional symbol-list @var{types}.
The default for @var{types} is an empty list, which will force any of those
commands in @var{music} to be interpreted as @code{\\repeat unfold}.  Possible
entries are @code{volta}, @code{tremolo} or @code{percent}.  Multiple entries
are possible.")
   (unfold-repeats types music))

voices =
#(define-music-function (ids music) (key-list? ly:music?)
   (_i "Take the given key list of numbers (indicating the use of
@samp{\\voiceOne}@dots{}) or symbols (indicating voice names,
typically converted from strings by argument list processing)
and assign the following @code{\\\\}-separated music to
contexts according to that list.  Named rather than numbered
contexts can be used for continuing one voice (for the sake of
spanners and lyrics), usually requiring a @code{\\voiceOne}-style
override at the beginning of the passage and a @code{\\oneVoice}
override at its end.

The default
@example
<< @dots{} \\\\ @dots{} \\\\ @dots{} >>
@end example
construct would correspond to
@example
\\voices 1,2,3 << @dots{} \\\\ @dots{} \\\\ @dots{} >>
@end example")
   (voicify-music music ids))

void =
#(define-void-function (arg) (scheme?)
   (_i "Accept a scheme argument, return a void expression.
Use this if you want to have a scheme expression evaluated
because of its side-effects, but its value ignored."))

volta =
#(define-music-function (volta-numbers music) (number-list? ly:music?)
   (_i "Mark @var{music} as being limited to the volte given in
@var{volta-numbers} when the innermost enclosing repeat is unfolded.
Volta number begins at 1 and increases by 1 with each repetition.")
   (volta-spec-music volta-numbers music))

vshape =
#(define-music-function (offsets item)
   (list? key-list-or-music?)
   "Like @code{\\shape}, but additionally show control points for
ease of tweaking."
   (once (propertyTweak 'show-control-points #t (shape offsets item))))

withMusicProperty =
#(define-music-function (sym val music)
   (symbol? scheme? ly:music?)
   (_i "Set @var{sym} to @var{val} in @var{music}.")

   (set! (ly:music-property music sym) val)
   music)
