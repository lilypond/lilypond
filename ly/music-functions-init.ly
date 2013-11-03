%%%% -*- Mode: Scheme -*-

%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2003--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.17.11"


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this file is alphabetically sorted.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% need SRFI-1 for filter; optargs for lambda*
#(use-modules (srfi srfi-1)
	      (ice-9 optargs))

%% TODO: using define-music-function in a .scm causes crash.

absolute =
#(define-music-function (parser location music)
   (ly:music?)
   (_i "Make @var{music} absolute.  This does not actually change the
music itself but rather hides it from surrounding @code{\\relative}
commands.")
   (make-music 'RelativeOctaveMusic 'element music))

acciaccatura =
#(def-grace-function startAcciaccaturaMusic stopAcciaccaturaMusic
   (_i "Create an acciaccatura from the following music expression"))

%% keep these two together
instrument-definitions = #'()
addInstrumentDefinition =
#(define-void-function
   (parser location name lst) (string? list?)
   (_i "Create instrument @var{name} with properties @var{list}.")
   (set! instrument-definitions (acons name lst instrument-definitions)))

addQuote =
#(define-void-function (parser location name music) (string? ly:music?)
   (_i "Define @var{music} as a quotable music expression named
@var{name}")
   (add-quotable parser name music))

%% keep these two together
afterGraceFraction = #(cons 6 8)
afterGrace =
#(define-music-function (parser location main grace) (ly:music? ly:music?)
   (_i "Create @var{grace} note(s) after a @var{main} music expression.")
   (let ((main-length (ly:music-length main))
	 (fraction  (ly:parser-lookup parser 'afterGraceFraction)))
     (make-simultaneous-music
      (list
       main
       (make-sequential-music
	(list

	 (make-music 'SkipMusic
		     'duration (ly:make-duration
				0 0
				(* (ly:moment-main-numerator main-length)
				   (car fraction))
				(* (ly:moment-main-denominator main-length)
				   (cdr fraction))))
	 (make-music 'GraceMusic
		     'element grace)))))))


%% music identifiers not allowed at top-level,
%% so this is a music-function instead.
allowPageTurn =
#(define-music-function (location parser) ()
   (_i "Allow a page turn. May be used at toplevel (ie between scores or
markups), or inside a score.")
   (make-music 'EventChord
	       'page-marker #t
	       'page-turn-permission 'allow
	       'elements (list (make-music 'PageTurnEvent
					   'break-permission 'allow))))

alterBroken =
#(define-music-function (parser location property arg item)
  (symbol-list-or-symbol? list? symbol-list-or-music?)
  (_i "Override @var{property} for pieces of broken spanner @var{item}
with values @var{arg}.  @var{item} may either be music in the form of
a starting spanner event, or a symbol list in the form
@samp{Context.Grob} or just @samp{Grob}.  Iff @var{item} is in the
form of a spanner event, @var{property} may also have the form
@samp{Grob.property} for specifying a directed tweak.")
  (if (ly:music? item)
      (if (eq? (ly:music-property item 'span-direction) START)
          #{ \tweak #property #(value-for-spanner-piece arg) #item #}
          (begin
            (ly:music-warning item (_ "not a spanner"))
            item))
      (let* ((p (check-grob-path item parser location
                                 #:default 'Bottom
                                 #:min 2
                                 #:max 2))
             (name (and p (second p)))
             (description
              (and name (assoc-get name all-grob-descriptions))))
        (if (and description
                 (member 'spanner-interface
                         (assoc-get 'interfaces
                                    (assoc-get 'meta description))))
            #{
              \override #item . #property =
              #(value-for-spanner-piece arg)
            #}
            (begin
              (ly:input-warning location (_ "not a spanner name, `~a'") name)
              (make-music 'Music))))))

appendToTag =
#(define-music-function (parser location tag more music)
   (symbol? ly:music? ly:music?)
   (_i "Append @var{more} to the @code{elements} of all music
expressions in @var{music} that are tagged with @var{tag}.")
   (music-map (lambda (m)
		(if (memq tag (ly:music-property m 'tags))
		    (set! (ly:music-property m 'elements)
			  (append (ly:music-property m 'elements)
				  (list more))))
		m)
	      music))

applyContext =
#(define-music-function (parser location proc) (procedure?)
   (_i "Modify context properties with Scheme procedure @var{proc}.")
   (make-music 'ApplyContext
	       'procedure proc))

applyMusic =
#(define-music-function (parser location func music) (procedure? ly:music?)
   (_i"Apply procedure @var{func} to @var{music}.")
   (func music))

applyOutput =
#(define-music-function (parser location ctx proc) (symbol? procedure?)
   (_i "Apply function @code{proc} to every layout object in context @code{ctx}")
   (make-music 'ApplyOutputEvent
	       'procedure proc
	       'context-type ctx))

appoggiatura =
#(def-grace-function startAppoggiaturaMusic stopAppoggiaturaMusic
   (_i "Create an appoggiatura from @var{music}"))

% for regression testing purposes.
assertBeamQuant =
#(define-music-function (parser location l r) (pair? pair?)
   (_i "Testing function: check whether the beam quants @var{l} and @var{r} are correct")
   (make-grob-property-override 'Beam 'positions (check-quant-callbacks l r)))

% for regression testing purposes.
assertBeamSlope =
#(define-music-function (parser location comp) (procedure?)
   (_i "Testing function: check whether the slope of the beam is the same as @code{comp}")
   (make-grob-property-override 'Beam 'positions (check-slope-callbacks comp)))

autochange =
#(define-music-function (parser location music) (ly:music?)
   (_i "Make voices that switch between staves automatically")
   (make-autochange-music parser music))



balloonGrobText =
#(define-music-function (parser location grob-name offset text)
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
#(define-event-function (parser location offset text) (number-pair? markup?)
   (_i "Attach @var{text} at @var{offset} (use like @code{\\tweak})")
   (make-music 'AnnotateOutputEvent
	       'X-offset (car offset)
	       'Y-offset (cdr offset)
	       'text text))

bar =
#(define-music-function (parser location type) (string?)
   (_i "Insert a bar line of type @var{type}")
   (context-spec-music
    (make-property-set 'whichBar type)
    'Timing))

barNumberCheck =
#(define-music-function (parser location n) (integer?)
   (_i "Print a warning if the current bar number is not @var{n}.")
   (make-music 'ApplyContext
	       'procedure
	       (lambda (c)
		 (let ((cbn (ly:context-property c 'currentBarNumber)))
		   (if (and  (number? cbn) (not (= cbn n)))
		       (ly:input-warning location
					 "Barcheck failed got ~a expect ~a"
					 cbn n))))))

bendAfter =
#(define-event-function (parser location delta) (real?)
   (_i "Create a fall or doit of pitch interval @var{delta}.")
   (make-music 'BendAfterEvent
	       'delta-step delta))

bookOutputName =
#(define-void-function (parser location newfilename) (string?)
   (_i "Direct output for the current book block to @var{newfilename}.")
   (set! (paper-variable parser #f 'output-filename) newfilename))

bookOutputSuffix =
#(define-void-function (parser location newsuffix) (string?)
   (_i "Set the output filename suffix for the current book block to
@var{newsuffix}.")
   (set! (paper-variable parser #f 'output-suffix) newsuffix))

%% \breathe is defined as a music function rather than an event identifier to
%% ensure it gets useful input location information: as an event identifier,
%% it would have to be wrapped in an EventChord to prevent it from being
%% treated as a post_event by the parser
breathe =
#(define-music-function (parser location) ()
   (_i "Insert a breath mark.")
   (make-music 'BreathingEvent))

clef =
#(define-music-function (parser location type) (string?)
   (_i "Set the current clef to @var{type}.")
   (make-clef-set type))


compoundMeter =
#(define-music-function (parser location args) (pair?)
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
    \once \override Staff.TimeSignature.stencil = #(lambda (grob)
      (grob-interpret-markup grob (format-compound-time args)))
    \set Timing.timeSignatureFraction = #timesig
    \set Timing.baseMoment = #beat
    \set Timing.beatStructure = #beatGrouping
    \set Timing.beamExceptions = #'()
    \set Timing.measureLength = #mlen
  #} ))

crossStaff =
#(define-music-function (parser location notes) (ly:music?)
  (_i "Create cross-staff stems")
  #{
  \temporary \override Stem.cross-staff = #cross-staff-connect
  \temporary \override Flag.style = #'no-flag
  #notes
  \revert Stem.cross-staff
  \revert Flag.style
#})

cueClef =
#(define-music-function (parser location type) (string?)
  (_i "Set the current cue clef to @var{type}.")
  (make-cue-clef-set type))

cueClefUnset =
#(define-music-function (parser location) ()
  (_i "Unset the current cue clef.")
  (make-cue-clef-unset))

cueDuring =
#(define-music-function
   (parser location what dir main-music) (string? ly:dir? ly:music?)
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
   (parser location what dir clef main-music) (string? ly:dir? string? ly:music?)
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
#(define-music-function (parser location music) (ly:music?)
   (_i "Display the LilyPond input representation of @var{music}
to the console.")
   (newline)
   (display-lily-music music parser)
   music)

displayMusic =
#(define-music-function (parser location music) (ly:music?)
   (_i "Display the internal representation of @var{music} to the console.")
   (newline)
   (display-scheme-music music)
   music)

displayScheme =
#(define-scheme-function (parser location expr) (scheme?)
   (_i "Display the internal representation of @var{expr} to the console.")
   (newline)
   (display-scheme-music expr)
   expr)



endSpanners =
#(define-music-function (parser location music) (ly:music?)
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
#(define-music-function (parser location music) (ly:music?)
   (_i "Compatibility function wrapping @code{EventChord} around
isolated rhythmic events occuring since version 2.15.28, after
expanding repeat chords @samp{q}.")
   (event-chord-wrap! music parser))

featherDurations=
#(define-music-function (parser location factor argument) (ly:moment? ly:music?)
   (_i "Adjust durations of music in @var{argument} by rational @var{factor}.")
   (let ((orig-duration (ly:music-length argument))
	 (multiplier (ly:make-moment 1 1)))

     (for-each
      (lambda (mus)
	(if (< 0 (ly:moment-main-denominator (ly:music-length mus)))
	    (begin
	      (ly:music-compress mus multiplier)
	      (set! multiplier (ly:moment-mul factor multiplier)))))
      (extract-named-music argument '(EventChord NoteEvent RestEvent SkipEvent)))
     (ly:music-compress
      argument
      (ly:moment-div orig-duration (ly:music-length argument)))

     argument))

finger =
#(define-event-function (parser location finger) (number-or-markup?)
   (_i "Apply @var{finger} as a fingering indication.")

   (make-music
            'FingeringEvent
            (if (number? finger) 'digit 'text)
            finger))

footnote =
#(define-music-function (parser location mark offset footnote item)
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
     #{ \once \tweak footnote-music #mus #item #}))

grace =
#(def-grace-function startGraceMusic stopGraceMusic
   (_i "Insert @var{music} as grace notes."))

grobdescriptions =
#(define-scheme-function (parser location descriptions) (list?)
   (_i "Create a context modification from @var{descriptions}, a list
in the format of @code{all-grob-descriptions}.")
   (ly:make-context-mod
    (map (lambda (p)
	   (list 'assign (car p) (list (cdr p))))
	 descriptions)))

harmonicByFret = #(define-music-function (parser location fret music) (number? ly:music?)
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

harmonicByRatio = #(define-music-function (parser location ratio music) (number? ly:music?)
    (_i "Convert @var{music} into mixed harmonics; the resulting notes resemble
harmonics played on a fretted instrument by touching the strings at the point
given through @var{ratio}.")
  #{
    \set harmonicDots = ##t
    \temporary \override TabNoteHead.stencil = #(tab-note-head::print-custom-fret-label (ratio->fret ratio))
    \temporary \override NoteHead.Y-extent = #(ly:make-unpure-pure-container ly:grob::stencil-height
                                       (lambda (grob start end)
                                               (ly:grob::stencil-height grob)))
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
#(define-music-function (parser location item) (symbol-list-or-music?)
   (_i "Set @var{item}'s @samp{transparent} property to @code{#t},
making it invisible while still retaining its dimensions.

If @var{item} is a symbol list of form @code{GrobName} or
@code{Context.GrobName}, the result is an override for the grob name
specified by it.  If @var{item} is a music expression, the result is
the same music expression with an appropriate tweak applied to it.")
   #{ \tweak transparent ##t #item #})

inStaffSegno =
#(define-music-function (parser location) ()
   (_i "Put the segno variant 'varsegno' at this position into the staff,
compatible with the repeat command.")
   (make-music 'ApplyContext
               'procedure
               (lambda (ctx)
                 (let ((score-ctx (ly:context-find ctx 'Score)))
                   (if (ly:context? score-ctx)
                     (let ((old-rc (ly:context-property score-ctx 'repeatCommands '())))
                       (if (eq? (memq 'segno-display old-rc) #f)
                         (ly:context-set-property! score-ctx 'repeatCommands (cons 'segno-display old-rc)))))))))

instrumentSwitch =
#(define-music-function
   (parser location name) (string?)
   (_i "Switch instrument to @var{name}, which must be predefined with
@code{\\addInstrumentDefinition}.")
   (let* ((handle (assoc name instrument-definitions))
	  (instrument-def (if handle (cdr handle) '())))

     (if (not handle)
	 (ly:input-warning location "No such instrument: ~a" name))
     (context-spec-music
      (make-music 'SimultaneousMusic
		  'elements
		  (map (lambda (kv)
			 (make-property-set
			  (car kv)
			  (cdr kv)))
		       instrument-def))
      'Staff)))



keepWithTag =
#(define-music-function (parser location tag music)
   (symbol-list-or-symbol? ly:music?)
   (_i "Include only elements of @var{music} that are either untagged
or tagged with one of the tags in @var{tag}.  @var{tag} may be either
a single symbol or a list of symbols.")
   (music-filter
    (if (symbol? tag)
        (lambda (m)
          (let ((music-tags (ly:music-property m 'tags)))
            (or (null? music-tags)
                (memq tag music-tags))))
        (lambda (m)
          (let ((music-tags (ly:music-property m 'tags)))
            (or (null? music-tags)
                (any (lambda (t) (memq t music-tags)) tag)))))
    music))

key =
#(define-music-function (parser location tonic pitch-alist)
   ((ly:pitch? '()) (list? '()))
   (_i "Set key to @var{tonic} and scale @var{pitch-alist}.
If both are null, just generate @code{KeyChangeEvent}.")
   (cond ((null? tonic) (make-music 'KeyChangeEvent))
	 ((null? pitch-alist)
	  (ly:parser-error parser (_ "second argument must be pitch list")
			   location)
	  (make-music 'SequentialMusic 'void #t))
	 (else
	  (ly:music-transpose
	   (make-music 'KeyChangeEvent
		'tonic (ly:make-pitch 0 0 0)
		'pitch-alist pitch-alist)
	   tonic))))

killCues =
#(define-music-function (parser location music) (ly:music?)
   (_i "Remove cue notes from @var{music}.")
   (music-map
    (lambda (mus)
      (if (and (string? (ly:music-property mus 'quoted-music-name))
	       (string=? (ly:music-property mus 'quoted-context-id "") "cue"))
	  (ly:music-property mus 'element)
	  mus))
    music))



label =
#(define-music-function (parser location label) (symbol?)
   (_i "Create @var{label} as a bookmarking label.")
   (make-music 'EventChord
	       'page-marker #t
	       'page-label label
	       'elements (list (make-music 'LabelEvent
					   'page-label label))))


language =
#(define-void-function (parser location language) (string?)
   (_i "Set note names for language @var{language}.")
   (note-names-language parser language))

languageSaveAndChange =
#(define-void-function (parser location language) (string?)
  (_i "Store the previous pitchnames alist, and set a new one.")
  (set! previous-pitchnames pitchnames)
  (note-names-language parser language))

languageRestore =
#(define-void-function (parser location) ()
   (_i "Restore a previously-saved pitchnames alist.")
   (if previous-pitchnames
       (begin
        (set! pitchnames previous-pitchnames)
        (ly:parser-set-note-names parser pitchnames))
      (ly:input-warning location (_ "No other language was defined previously. Ignoring."))))


makeClusters =
#(define-music-function (parser location arg) (ly:music?)
   (_i "Display chords in @var{arg} as clusters.")
   (music-map note-to-cluster arg))

modalInversion =
#(define-music-function (parser location around to scale music)
    (ly:pitch? ly:pitch? ly:music? ly:music?)
    (_i "Invert @var{music} about @var{around} using @var{scale} and
transpose from @var{around} to @var{to}.")
    (let ((inverter (make-modal-inverter around to scale)))
      (change-pitches music inverter)
      music))

modalTranspose =
#(define-music-function (parser location from to scale music)
    (ly:pitch? ly:pitch? ly:music? ly:music?)
    (_i "Transpose @var{music} from pitch @var{from} to pitch @var{to}
using @var{scale}.")
    (let ((transposer (make-modal-transposer from to scale)))
      (change-pitches music transposer)
      music))

inversion =
#(define-music-function
   (parser location around to music) (ly:pitch? ly:pitch? ly:music?)
   (_i "Invert @var{music} about @var{around} and
transpose from @var{around} to @var{to}.")
   (music-invert around to music))

mark =
#(define-music-function
   (parser location label) ((scheme? '()))
  "Make the music for the \\mark command."
  (let* ((set (and (integer? label)
		   (context-spec-music (make-property-set 'rehearsalMark label)
				      'Score)))
	 (ev (make-music 'MarkEvent
			 'origin location)))

    (if set
	(make-sequential-music (list set ev))
	(begin
	  (set! (ly:music-property ev 'label) label)
	  ev))))

musicMap =
#(define-music-function (parser location proc mus) (procedure? ly:music?)
   (_i "Apply @var{proc} to @var{mus} and all of the music it contains.")
   (music-map proc mus))

%% noPageBreak and noPageTurn are music functions (not music indentifiers),
%% because music identifiers are not allowed at top-level.
noPageBreak =
#(define-music-function (location parser) ()
   (_i "Forbid a page break.  May be used at toplevel (i.e., between scores or
markups), or inside a score.")
   (make-music 'EventChord
	       'page-marker #t
	       'page-break-permission 'forbid
	       'elements (list (make-music 'PageBreakEvent
					   'break-permission '()))))

noPageTurn =
#(define-music-function (location parser) ()
   (_i "Forbid a page turn.  May be used at toplevel (i.e., between scores or
markups), or inside a score.")
   (make-music 'EventChord
	       'page-marker #t
	       'page-turn-permission 'forbid
	       'elements (list (make-music 'PageTurnEvent
					   'break-permission '()))))



octaveCheck =
#(define-music-function (parser location pitch) (ly:pitch?)
   (_i "Octave check.")
   (make-music 'RelativeOctaveCheck
               'pitch pitch))

offset =
#(define-music-function (parser location property offsets item)
  (symbol-list-or-symbol? scheme? symbol-list-or-music?)
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
                         parser location
                         #:start 1 #:default #t #:min 2 #:max 2)))
        (if prop-path
            ; If the head of the grob property path is a symbol--i.e.,
            ; a grob name, produce a directed tweak.  Otherwise, create
            ; an ordinary tweak.
            (if (symbol? (car prop-path))
                #{
                  \tweak #prop-path #(offsetter (second prop-path) offsets) #item
                #}
                #{
                  \tweak #(second prop-path) #(offsetter (second prop-path) offsets) #item
                #})
            item))
      ; In case of an override, grob property path is Context.Grob.property.
      (let ((prop-path (check-grob-path
                         (append item
                                 (if (symbol? property)
                                     (list property)
                                     property))
                         parser location
                         #:default 'Bottom #:min 3 #:max 3)))
        (if prop-path
            #{
              \override #prop-path = #(offsetter (third prop-path) offsets)
            #}
            (make-music 'Music)))))
 
omit =
#(define-music-function (parser location item) (symbol-list-or-music?)
   (_i "Set @var{item}'s @samp{stencil} property to @code{#f},
effectively omitting it without taking up space.

If @var{item} is a symbol list of form @code{GrobName} or
@code{Context.GrobName}, the result is an override for the grob name
specified by it.  If @var{item} is a music expression, the result is
the same music expression with an appropriate tweak applied to it.")
   #{ \tweak stencil ##f #item #})

once =
#(define-music-function (parser location music) (ly:music?)
   (_i "Set @code{once} to @code{#t} on all layout instruction events
in @var{music}.  This will complain about music with an actual
duration.  As a special exception, if @var{music} contains
@samp{tweaks} it will be silently ignored in order to allow for
@code{\\once \\tweak} to work as both one-time override and proper
tweak.")
   (if (not (pair? (ly:music-property music 'tweaks)))
       (for-some-music
        (lambda (m)
          (cond ((music-is-of-type? m 'layout-instruction-event)
                 (set! (ly:music-property m 'once) #t)
                 #t)
                ((ly:duration? (ly:music-property m 'duration))
                 (ly:music-warning m (_ "Cannot apply \\once to timed music"))
                 #t)
                (else #f)))
        music))
   music)

ottava =
#(define-music-function (parser location octave) (integer?)
   (_i "Set the octavation.")
   (make-music 'OttavaMusic
	       'ottava-number octave))

overrideTimeSignatureSettings =
#(define-music-function
   (parser location time-signature base-moment beat-structure beam-exceptions)
   (fraction? fraction? list? list?)

   (_i "Override @code{timeSignatureSettings}
for time signatures of @var{time-signature} to have settings
of @var{base-moment}, @var{beat-structure}, and @var{beam-exceptions}.")

   ;; TODO -- add warning if largest value of grouping is
   ;;	    greater than time-signature.
  (let ((setting (make-setting base-moment beat-structure beam-exceptions)))
    (override-time-signature-setting time-signature setting)))

overrideProperty =
#(define-music-function (parser location grob-property-path value)
   (symbol-list? scheme?)

   (_i "Set the grob property specified by @var{grob-property-path} to
@var{value}.  @var{grob-property-path} is a symbol list of the form
@code{Context.GrobName.property} or @code{GrobName.property}, possibly
with subproperties given as well.")
   (let ((p (check-grob-path grob-property-path parser location
                             #:default 'Bottom
                             #:min 3)))
     (if p
         (make-music 'ApplyOutputEvent
                     'context-type (first p)
                     'procedure
                     (lambda (grob orig-context context)
                       (if (equal?
                            (cdr (assoc 'name (ly:grob-property grob 'meta)))
                            (second p))
                           (ly:grob-set-nested-property!
                            grob (cddr p) value))))
         (make-music 'Music))))






%% pageBreak and pageTurn are music functions (iso music indentifiers),
%% because music identifiers are not allowed at top-level.
pageBreak =
#(define-music-function (location parser) ()
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
#(define-music-function (location parser) ()
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
#(define-void-function (parser location voice-ids music) (list? ly:music?)
   (_i "Define parallel music sequences, separated by '|' (bar check signs),
and assign them to the identifiers provided in @var{voice-ids}.

@var{voice-ids}: a list of music identifiers (symbols containing only letters)

@var{music}: a music sequence, containing BarChecks as limiting expressions.

Example:

@verbatim
  \\parallelMusic #'(A B C) {
    c c | d d | e e |
    d d | e e | f f |
  }
<==>
  A = { c c | d d | }
  B = { d d | e e | }
  C = { e e | f f | }
@end verbatim
")
   (define (bar-check? m)
     "Checks whether m is a bar check."
     (eq? (ly:music-property m 'name) 'BarCheck))
   (define (recurse-and-split music)
     "This returns either a list of music split along barchecks, or
@code{#f}."
     (let ((elt (ly:music-property music 'element))
           (elts (ly:music-property music 'elements)))
       (cond ((ly:music? elt)
              (let ((lst (recurse-and-split elt)))
                (and lst
                     (map
                      (lambda (x)
                        (let ((res (music-clone music 'element x)))
                          (if (ly:input-location?
                               (ly:music-property x 'origin))
                              (set! (ly:music-property res 'origin)
                                    (ly:music-property x 'origin)))
                          res))
                      lst))))
             ((any bar-check? elts)
              (let* ((voices (apply circular-list
                                    (make-list (length voice-ids)
                                               '())))
                     (current-voices voices)
                     (current-sequence '()))
                ;;
                ;; utilities
                (define (push-music m)
                  "Push the music expression into the current sequence"
                  (set! current-sequence (cons m current-sequence)))
                (define (change-voice)
                  "Stores the previously built sequence into the current voice and
       change to the following voice."
                  (set-car! current-voices
                            (cons (reverse! current-sequence)
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
                          elts)
                (if (pair? current-sequence) (change-voice))
                ;; un-circularize `voices' and reorder the voices

                (set! voices (map reverse!
                                  (list-head voices (length voice-ids))))

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
                                                     (_ "Bars in parallel music don't have the same length"))))
                                              seqs)))
                       voices)
                (map
                 (lambda (lst)
                   (set! lst (concatenate! lst))
                   (let ((res (music-clone music 'elements lst)))
                     (if (and (pair? lst)
                              (ly:input-location? (ly:music-property
                                                   (car lst)
                                                   'origin)))
                         (set! (ly:music-property res 'origin)
                               (ly:music-property (car lst) 'origin)))
                     res))
                 voices)))
             (else #f))))
   (let ((voices (recurse-and-split music)))
     (if voices
         ;;
         ;; bind voice identifiers to the voices
         (for-each (lambda (voice-id voice)
                     (ly:parser-define! parser voice-id voice))
	  voice-ids voices)
         (ly:music-warning music
                           (_ "ignoring parallel music without barchecks")))))

parenthesize =
#(define-music-function (parser loc arg) (ly:music?)
   (_i "Tag @var{arg} to be parenthesized.")

   (if (memq 'event-chord (ly:music-property arg 'types))
       ;; arg is an EventChord -> set the parenthesize property
       ;; on all child notes and rests
       (for-each
	(lambda (ev)
	  (if (or (memq 'note-event (ly:music-property ev 'types))
		  (memq 'rest-event (ly:music-property ev 'types)))
	      (set! (ly:music-property ev 'parenthesize) #t)))
	(ly:music-property arg 'elements))
       ;; No chord, simply set property for this expression:
       (set! (ly:music-property arg 'parenthesize) #t))
   arg)

partcombine =
#(define-music-function (parser location part1 part2) (ly:music? ly:music?)
   (_i "Take the music in @var{part1} and @var{part2} and typeset so
that they share a staff.")
   (make-part-combine-music parser
                            (list part1 part2) #f))

partcombineUp =
#(define-music-function (parser location part1 part2) (ly:music? ly:music?)
   (_i "Take the music in @var{part1} and @var{part2} and typeset so
that they share a staff with stems directed upward.")
   (make-part-combine-music parser
                            (list part1 part2) UP))

partcombineDown =
#(define-music-function (parser location part1 part2) (ly:music? ly:music?)
   (_i "Take the music in @var{part1} and @var{part2} and typeset so
that they share a staff with stems directed downward.")
   (make-part-combine-music parser
                            (list part1 part2) DOWN))

partcombineForce =
#(define-music-function (location parser type once) (symbol-or-boolean? boolean?)
   (_i "Override the part-combiner.")
   (make-music 'EventChord
	       'elements (list (make-music 'PartCombineForceEvent
					   'forced-type type
					   'once once))))
partcombineApart = \partcombineForce #'apart ##f
partcombineApartOnce = \partcombineForce #'apart ##t
partcombineChords = \partcombineForce #'chords ##f
partcombineChordsOnce = \partcombineForce #'chords ##t
partcombineUnisono = \partcombineForce #'unisono ##f
partcombineUnisonoOnce = \partcombineForce #'unisono ##t
partcombineSoloI = \partcombineForce #'solo1 ##f
partcombineSoloIOnce = \partcombineForce #'solo1 ##t
partcombineSoloII = \partcombineForce #'solo2 ##f
partcombineSoloIIOnce = \partcombineForce #'solo2 ##t
partcombineAutomatic = \partcombineForce ##f ##f
partcombineAutomaticOnce = \partcombineForce ##f ##t

partial =
#(define-music-function (parser location dur) (ly:duration?)
  (_i "Make a partial measure.")

  ;; We use `descend-to-context' here instead of `context-spec-music' to
  ;; ensure \partial still works if the Timing_translator is moved
    (descend-to-context
     (context-spec-music (make-music 'PartialSet
				     'origin location
				     'duration dur)
			 'Timing)
     'Score))

pitchedTrill =
#(define-music-function
   (parser location main-note secondary-note)
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
                   (ly:input-warning location (_ "Second argument of \\pitchedTrill should be single note: "))
                   (display sec-note-events)))

             (if (eq? forced #t)
                 (for-each (lambda (m)
                             (ly:music-set-property! m 'force-accidental forced))
                           trill-events)))))
     main-note))

pushToTag =
#(define-music-function (parser location tag more music)
   (symbol? ly:music? ly:music?)
   (_i "Add @var{more} to the front of @code{elements} of all music
expressions in @var{music} that are tagged with @var{tag}.")
   (music-map (lambda (m)
		(if (memq tag (ly:music-property m 'tags))
		    (set! (ly:music-property m 'elements)
			  (cons more (ly:music-property m 'elements))))
		m)
	      music))

quoteDuring =
#(define-music-function (parser location what main-music) (string? ly:music?)
   (_i "Indicate a section of music to be quoted.  @var{what} indicates the name
of the quoted voice, as specified in an @code{\\addQuote} command.
@var{main-music} is used to indicate the length of music to be quoted;
usually contains spacers or multi-measure rests.")
   (make-music 'QuoteMusic
               'element main-music
               'quoted-music-name what))

relative =
#(define-music-function (parser location pitch music)
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
#(define-music-function (parser location tag music)
   (symbol-list-or-symbol? ly:music?)
   (_i "Remove elements of @var{music} that are tagged with one of the
tags in @var{tag}.  @var{tag} may be either a single symbol or a list
of symbols.")
   (music-filter
    (if (symbol? tag)
        (lambda (m)
          (not (memq tag (ly:music-property m 'tags))))
        (lambda (m)
          (let ((music-tags (ly:music-property m 'tags)))
            (or (null? music-tags)
                (not (any (lambda (t) (memq t music-tags)) tag))))))
    music))

resetRelativeOctave =
#(define-music-function (parser location pitch) (ly:pitch?)
   (_i "Set the octave inside a \\relative section.")

   (make-music 'SequentialMusic
               'to-relative-callback
               (lambda (music last-pitch) pitch)))

retrograde =
#(define-music-function (parser location music)
    (ly:music?)
    (_i "Return @var{music} in reverse order.")
    (retrograde-music music))

revertTimeSignatureSettings =
#(define-music-function
   (parser location time-signature)
   (pair?)

   (_i "Revert @code{timeSignatureSettings}
for time signatures of @var{time-signature}.")
   (revert-time-signature-setting time-signature))

rightHandFinger =
#(define-event-function (parser location finger) (number-or-markup?)
   (_i "Apply @var{finger} as a fingering indication.")

   (make-music
            'StrokeFingerEvent
            (if (number? finger) 'digit 'text)
            finger))

scaleDurations =
#(define-music-function (parser location fraction music)
   (fraction? ly:music?)
   (_i "Multiply the duration of events in @var{music} by @var{fraction}.")
   (ly:music-compress music
		      (ly:make-moment (car fraction) (cdr fraction))))

settingsFrom =
#(define-scheme-function (parser location ctx music)
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
#(define-music-function (parser location offsets item)
   (list? symbol-list-or-music?)
   (_i "Offset control-points of @var{item} by @var{offsets}.  The
argument is a list of number pairs or list of such lists.  Each
element of a pair represents an offset to one of the coordinates of a
control-point.  If @var{item} is a string, the result is
@code{\\once\\override} for the specified grob type.  If @var{item} is
a music expression, the result is the same music expression with an
appropriate tweak applied.")
   (define (shape-curve grob)
     (let* ((orig (ly:grob-original grob))
            (siblings (if (ly:spanner? grob)
                          (ly:spanner-broken-into orig) '()))
            (total-found (length siblings))
            (function (assoc-get 'control-points
                                 (reverse (ly:grob-basic-properties grob))))
            (coords (function grob)))

       (define (offset-control-points offsets)
         (if (null? offsets)
             coords
             (map
               (lambda (x y) (coord-translate x y))
               coords offsets)))

       (define (helper sibs offs)
         (if (pair? offs)
             (if (eq? (car sibs) grob)
                 (offset-control-points (car offs))
                 (helper (cdr sibs) (cdr offs)))
             coords))

       ;; we work with lists of lists
       (if (or (null? offsets)
               (not (list? (car offsets))))
           (set! offsets (list offsets)))

       (if (>= total-found 2)
           (helper siblings offsets)
           (offset-control-points (car offsets)))))
   #{ \once \tweak control-points #shape-curve #item #})

shiftDurations =
#(define-music-function (parser location dur dots arg)
   (integer? integer? ly:music?)
   (_i "Change the duration of @var{arg} by adding @var{dur} to the
@code{durlog} of @var{arg} and @var{dots} to the @code{dots} of @var{arg}.")

   (music-map
    (lambda (x)
      (shift-one-duration-log x dur dots)) arg))

single =
#(define-music-function (parser location overrides music)
   (ly:music? ly:music?)
   (_i "Convert @var{overrides} to tweaks and apply them to @var{music}.
This does not convert @code{\\revert}, @code{\\set} or @code{\\unset}.")
   (set! (ly:music-property music 'tweaks)
         (fold-some-music
          (lambda (m) (eq? (ly:music-property m 'name)
                           'OverrideProperty))
          (lambda (m tweaks)
            (let ((p (cond
                      ((ly:music-property m 'grob-property #f) => list)
                      (else
                       (ly:music-property m 'grob-property-path)))))
              (acons (cons (ly:music-property m 'symbol) ;grob name
                           (if (pair? (cdr p))
                               p ;grob property path
                               (car p))) ;grob property
                     (ly:music-property m 'grob-value)
                     tweaks)))
          (ly:music-property music 'tweaks)
          overrides))
   music)

skip =
#(define-music-function (parser location dur) (ly:duration?)
  (_i "Skip forward by @var{dur}.")
  (make-music 'SkipMusic
	      'duration dur))


slashedGrace =
#(def-grace-function startSlashedGraceMusic stopSlashedGraceMusic
   (_i "Create slashed graces (slashes through stems, but no slur) from
the following music expression"))

spacingTweaks =
#(define-music-function (parser location parameters) (list?)
   (_i "Set the system stretch, by reading the 'system-stretch property of
the `parameters' assoc list.")
   #{
     \overrideProperty Score.NonMusicalPaperColumn.line-break-system-details
     #(list (cons 'alignment-extra-space (cdr (assoc 'system-stretch parameters)))
	     (cons 'system-Y-extent (cdr (assoc 'system-Y-extent parameters))))
   #})

styledNoteHeads =
#(define-music-function (parser location style heads music)
   (symbol? symbol-list-or-symbol? ly:music?)
   (_i "Set @var{heads} in @var{music} to @var{style}.")
   (style-note-heads heads style music))

tag =
#(define-music-function (parser location tag music) (symbol-list-or-symbol? ly:music?)
   (_i "Tag the following @var{music} with @var{tag} and return the
result, by adding the single symbol or symbol list @var{tag} to the
@code{tags} property of @var{music}.")

   (set!
    (ly:music-property music 'tags)
    ((if (symbol? tag) cons append)
     tag
     (ly:music-property music 'tags)))
   music)

temporary =
#(define-music-function (parser location music)
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
                    (ly:input-warning location (_ "Cannot make ~a revertible")
                                      (ly:music-property m 'name))
                    (set! warned #t)))
              #t))))
    music)
   music)

time =
#(define-music-function (parser location beat-structure fraction)
   ((number-list? '()) fraction?)
   (_i "Set @var{fraction} as time signature, with optional
number list @var{beat-structure} before it.")
  (make-music 'TimeSignatureMusic
              'numerator (car fraction)
              'denominator (cdr fraction)
              'beat-structure beat-structure))

times =
#(define-music-function (parser location fraction music)
   (fraction? ly:music?)
   (_i "Scale @var{music} in time by @var{fraction}.")
  (make-music 'TimeScaledMusic
  	      'element (ly:music-compress music (ly:make-moment (car fraction) (cdr fraction)))
  	      'numerator (car fraction)
  	      'denominator (cdr fraction)))

transpose =
#(define-music-function
   (parser location from to music)
   (ly:pitch? ly:pitch? ly:music?)

   (_i "Transpose @var{music} from pitch @var{from} to pitch @var{to}.")
   (make-music 'TransposedMusic
               'element (ly:music-transpose music (ly:pitch-diff to from))))

transposedCueDuring =
#(define-music-function
   (parser location what dir pitch main-music)
   (string? ly:dir? ly:pitch? ly:music?)

   (_i "Insert notes from the part @var{what} into a voice called @code{cue},
using the transposition defined by @var{pitch}.  This happens
simultaneously with @var{main-music}, which is usually a rest.	The
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
#(define-music-function (parser location pitch) (ly:pitch?)
   (_i "Set instrument transposition")

   (context-spec-music
    (make-property-set 'instrumentTransposition pitch)
    'Staff))

tuplet =
#(define-music-function (parser location ratio tuplet-span music)
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
#(define-music-function (parser location tuplet-span)
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
#(define-music-function (parser location prop value item)
   (symbol-list-or-symbol? scheme? symbol-list-or-music?)
   (_i "Add a tweak to the following @var{item}, usually music.
Layout objects created by @var{item} get their property @var{prop}
set to @var{value}.  If @var{prop} has the form @samp{Grob.property}, like with
@example
\\tweak Accidental.color #red cis'
@end example
an indirectly created grob (@samp{Accidental} is caused by
@samp{NoteHead}) can be tweaked; otherwise only directly created grobs
are affected.

As a special case, @var{item} may be a symbol list specifying a grob
path, in which case @code{\\override} is called on it instead of
creating tweaked music.  This is mainly useful when using
@code{\\tweak} as as a component for building other functions.

If this use case would call for @code{\\once \\override} rather than a
plain @code{\\override}, writing @code{\\once \\tweak @dots{}} can be
convenient.

@var{prop} can contain additional elements in which case a nested
property (inside of an alist) is tweaked.")
   (if (ly:music? item)
       (let ((p (check-grob-path prop parser location
                                 #:start 1
                                 #:default #t
                                 #:min 2)))
         (if p
             (set! (ly:music-property item 'tweaks)
                   (acons (cond ((pair? (cddr p)) p)
                                ((symbol? (car p))
                                 (cons (car p) (cadr p)))
                                (else (cadr p)))
                          value
                          (ly:music-property item 'tweaks))))
         item)
       ;; We could just throw this at \override and let it sort this
       ;; out on its own, but this way we should get better error
       ;; diagnostics.
       (let ((p (check-grob-path
                 (append item (if (symbol? prop) (list prop) prop))
                 parser location
                 #:default 'Bottom #:min 3)))
         (if p
             #{ \override #p = #value #}
             (make-music 'Music)))))

undo =
#(define-music-function (parser location music)
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
            (lambda (m) (or (music-is-of-type? m 'layout-instruction-event)
                            (music-is-of-type? m 'context-specification)
                            (music-is-of-type? m 'apply-context)
                            (music-is-of-type? m 'time-signature-music)))
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
                       (ly:input-warning location (_ "Cannot revert ~a")
                                         (ly:music-property m 'name))
                       (set! warned #t)))
                 overrides)))
            '()
            music)))
       (cond
        ((null? lst) (make-music 'Music))
        ((null? (cdr lst)) (car lst))
        (else (make-sequential-music lst))))))

unfoldRepeats =
#(define-music-function (parser location music) (ly:music?)
   (_i "Force any @code{\\repeat volta}, @code{\\repeat tremolo} or
@code{\\repeat percent} commands in @var{music} to be interpreted
as @code{\\repeat unfold}.")
   (unfold-repeats music))

void =
#(define-void-function (parser location arg) (scheme?)
   (_i "Accept a scheme argument, return a void expression.
Use this if you want to have a scheme expression evaluated
because of its side-effects, but its value ignored."))

withMusicProperty =
#(define-music-function (parser location sym val music)
   (symbol? scheme? ly:music?)
   (_i "Set @var{sym} to @var{val} in @var{music}.")

   (set! (ly:music-property music sym) val)
   music)
