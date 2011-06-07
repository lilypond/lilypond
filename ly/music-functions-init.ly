%%%% -*- Mode: Scheme -*-

%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2003--2011 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.14.0"


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this file is alphabetically sorted.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% need SRFI-1 for filter; optargs for lambda*
#(use-modules (srfi srfi-1)
	      (ice-9 optargs))

%% TODO: using define-music-function in a .scm causes crash.

acciaccatura =
#(def-grace-function startAcciaccaturaMusic stopAcciaccaturaMusic
   (_i "Create an acciaccatura from the following music expression"))

%% keep these two together
"instrument-definitions" = #'()
addInstrumentDefinition =
#(define-music-function
   (parser location name lst) (string? list?)
   (_i "Create instrument @var{name} with properties @var{list}.")
   (set! instrument-definitions (acons name lst instrument-definitions))
   (make-music 'SequentialMusic 'void #t))

addQuote =
#(define-music-function (parser location name music) (string? ly:music?)
   (_i "Define @var{music} as a quotable music expression named
@var{name}")
   (add-quotable parser name music)
   (make-music 'SequentialMusic 'void #t))

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
   (make-grob-property-override 'Beam 'positions
				(ly:make-simple-closure
				 (ly:make-simple-closure
				  (append
				   (list chain-grob-member-functions `(,cons 0 0))
				   (check-quant-callbacks l r))))))

% for regression testing purposes.
assertBeamSlope =
#(define-music-function (parser location comp) (procedure?)
   (_i "Testing function: check whether the slope of the beam is the same as @code{comp}")
   (make-grob-property-override 'Beam 'positions
				(ly:make-simple-closure
				 (ly:make-simple-closure
				  (append
				   (list chain-grob-member-functions `(,cons 0 0))
				   (check-slope-callbacks comp))))))

autochange =
#(define-music-function (parser location music) (ly:music?)
   (_i "Make voices that switch between staves automatically")
   (make-autochange-music parser music))



balloonGrobText =
#(define-music-function (parser location grob-name offset text)
   (symbol? number-pair? markup?)
   (_i "Attach @var{text} to @var{grob-name} at offset @var{offset}
 (use like @code{\\once})")
   (make-music 'AnnotateOutputEvent
	       'symbol grob-name
	       'X-offset (car offset)
	       'Y-offset (cdr offset)
	       'text text))

balloonText =
#(define-music-function (parser location offset text) (number-pair? markup?)
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
		       (ly:input-message location
					 "Barcheck failed got ~a expect ~a"
					 cbn n))))))

bendAfter =
#(define-music-function (parser location delta) (real?)
   (_i "Create a fall or doit of pitch interval @var{delta}.")
   (make-music 'BendAfterEvent
	       'delta-step delta))

bookOutputName =
#(define-music-function (parser location newfilename) (string?)
   (_i "Direct output for the current book block to @var{newfilename}.")
   (set! book-filename newfilename)
   (make-music 'SequentialMusic 'void #t))

bookOutputSuffix =
#(define-music-function (parser location newsuffix) (string?)
   (_i "Set the output filename suffix for the current book block to
@var{newsuffix}.")
   (set! book-output-suffix newsuffix)
   (make-music 'SequentialMusic 'void #t))

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
    \once \override Staff.TimeSignature #'stencil = #(lambda (grob)
		(grob-interpret-markup grob (format-compound-time $args)))
    \set Timing.timeSignatureFraction = $timesig
    \set Timing.baseMoment = $beat
    \set Timing.beatStructure = $beatGrouping
    \set Timing.beamExceptions = #'()
    \set Timing.measureLength = $mlen
  #} ))


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
	       'quoted-context-type 'Voice
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
	       'quoted-context-type 'Voice
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



endSpanners =
#(define-music-function (parser location music) (ly:music?)
   (_i "Terminate the next spanner prematurely after exactly one note
without the need of a specific end spanner.")
   (if (eq? (ly:music-property music 'name) 'EventChord)
       (let* ((elts (ly:music-property music 'elements))
	      (start-span-evs (filter (lambda (ev)
					(and (music-has-type ev 'span-event)
					     (equal? (ly:music-property ev 'span-direction)
						     START)))
				      elts))
	      (stop-span-evs
	       (map (lambda (m)
		      (let ((c (music-clone m)))
			(set! (ly:music-property c 'span-direction) STOP)
			c))
		    start-span-evs))
	      (end-ev-chord (make-music 'EventChord
					'elements stop-span-evs))
	      (total (make-music 'SequentialMusic
				 'elements (list music
						 end-ev-chord))))
	 total)

       (ly:input-message location (_ "argument endSpanners is not an EventChord: ~a" music))))



featherDurations=
#(define-music-function (parser location factor argument) (ly:moment? ly:music?)
   (_i "Adjust durations of music in @var{argument} by rational @var{factor}.")
   (let ((orig-duration (ly:music-length argument))
	 (multiplier (ly:make-moment 1 1)))

     (music-map
      (lambda (mus)
	(if (and (eq? (ly:music-property mus 'name) 'EventChord)
		 (< 0 (ly:moment-main-denominator (ly:music-length mus))))
	    (begin
	      (ly:music-compress mus multiplier)
	      (set! multiplier (ly:moment-mul factor multiplier))))
	mus)
      argument)

     (ly:music-compress
      argument
      (ly:moment-div orig-duration (ly:music-length argument)))

     argument))

footnoteGrob =
#(define-music-function (parser location grob-name offset text footnote)
   (symbol? number-pair? markup? markup?)
   (_i "Attach @var{text} to @var{grob-name} at offset @var{offset},
 with @var{text} referring to @var{footnote} (use like @code{\\once})")
   (make-music 'FootnoteEvent
	       'symbol grob-name
	       'X-offset (car offset)
	       'Y-offset (cdr offset)
	       'text text
	       'footnote-text footnote))

footnote =
#(define-music-function (parser location offset text footnote)
   (number-pair? markup? markup?)
   (_i "Attach @var{text} at @var{offset} with @var{text} referring
 to @var{footnote} (use like @code{\\tweak})")
   (make-music 'FootnoteEvent
	       'X-offset (car offset)
	       'Y-offset (cdr offset)
	       'text text
	       'footnote-text footnote))

grace =
#(def-grace-function startGraceMusic stopGraceMusic
   (_i "Insert @var{music} as grace notes."))

harmonicByFret = #(define-music-function (parser location fret music) (number? ly:music?)
  (let* ((fret (number->string fret))
         (pitch (fret->pitch fret)))
        (make-sequential-music
         (list
          #{
            \override TabNoteHead #'stencil = #(tab-note-head::print-custom-fret-label $fret)
          #}
          (make-harmonic
            (calc-harmonic-pitch pitch music))
          #{
            \revert TabNoteHead #'stencil
          #}))))

harmonicByRatio = #(define-music-function (parser location ratio music) (number? ly:music?)
  (let ((pitch (ratio->pitch ratio))
        (fret (ratio->fret ratio)))
       (make-sequential-music
        (list
         #{
           \override TabNoteHead #'stencil = #(tab-note-head::print-custom-fret-label $fret)
         #}
         (make-harmonic
           (calc-harmonic-pitch pitch music))
         #{
            \revert TabNoteHead #'stencil
         #}))))

instrumentSwitch =
#(define-music-function
   (parser location name) (string?)
   (_i "Switch instrument to @var{name}, which must be predefined with
@code{\\addInstrumentDefinition}.")
   (let* ((handle (assoc name instrument-definitions))
	  (instrument-def (if handle (cdr handle) '())))

     (if (not handle)
	 (ly:input-message location "No such instrument: ~a" name))
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
#(define-music-function (parser location tag music) (symbol? ly:music?)
   (_i "Include only elements of @var{music} that are tagged with @var{tag}.")
   (music-filter
    (lambda (m)
      (let* ((tags (ly:music-property m 'tags))
	     (res (memq tag tags)))
	(or
	 (eq? tags '())
	 res)))
    music))

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
#(define-music-function (parser location language) (string?)
   (_i "Set note names for language @var{language}.")
   (note-names-language parser language)
   (make-music 'Music 'void #t))

languageSaveAndChange =
#(define-music-function (parser location language) (string?)
  (_i "Store the previous pitchnames alist, and set a new one.")
  (set! previous-pitchnames pitchnames)
  (note-names-language parser language)
  (make-music 'Music 'void #t))

languageRestore =
#(define-music-function (parser location) ()
   (_i "Restore a previously-saved pitchnames alist.")
   (if previous-pitchnames
       (begin
        (set! pitchnames previous-pitchnames)
        (ly:parser-set-note-names parser pitchnames))
      (ly:warning (_ "No other language was defined previously. Ignoring.")))
   (make-music 'Music 'void #t))


makeClusters =
#(define-music-function (parser location arg) (ly:music?)
   (_i "Display chords in @var{arg} as clusters.")
   (music-map note-to-cluster arg))

modalInversion =
#(define-music-function (parser location around to scale music)
    (ly:music? ly:music? ly:music? ly:music?)
    (_i "Invert @var{music} about @var{around} using @var{scale} and
transpose from @var{around} to @var{to}.")
    (let ((inverter (make-modal-inverter around to scale)))
      (change-pitches music inverter)
      music))

modalTranspose =
#(define-music-function (parser location from to scale music)
    (ly:music? ly:music? ly:music? ly:music?)
    (_i "Transpose @var{music} from pitch @var{from} to pitch @var{to}
using @var{scale}.")
    (let ((transposer (make-modal-transposer from to scale)))
      (change-pitches music transposer)
      music))

inversion =
#(define-music-function
   (parser location around to music) (ly:music? ly:music? ly:music?)
   (_i "Invert @var{music} about @var{around} and
transpose from @var{around} to @var{to}.")
   (music-invert around to music))

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
#(define-music-function (parser location pitch-note) (ly:music?)
   (_i "Octave check.")
   (make-music 'RelativeOctaveCheck
	       'pitch (pitch-of-note pitch-note)))

ottava =
#(define-music-function (parser location octave) (integer?)
   (_i "Set the octavation.")
   (make-music 'OttavaMusic
	       'ottava-number octave))

overrideTimeSignatureSettings =
#(define-music-function
   (parser location time-signature base-moment beat-structure beam-exceptions)
   (pair? pair? cheap-list? cheap-list?)

   (_i "Override @code{timeSignatureSettings}
for time signatures of @var{time-signature} to have settings
of @var{base-moment}, @var{beat-structure}, and @var{beam-exceptions}.")

   ;; TODO -- add warning if largest value of grouping is
   ;;	    greater than time-signature.
  (let ((setting (make-setting base-moment beat-structure beam-exceptions)))
    (override-time-signature-setting time-signature setting)))

overrideProperty =
#(define-music-function (parser location name property value)
   (string? symbol? scheme?)

   (_i "Set @var{property} to @var{value} in all grobs named @var{name}.
The @var{name} argument is a string of the form @code{\"Context.GrobName\"}
or @code{\"GrobName\"}.")

   (let ((name-components (string-split name #\.))
	 (context-name 'Bottom)
	 (grob-name #f))

     (if (> 2 (length name-components))
	 (set! grob-name (string->symbol (car name-components)))
	 (begin
	   (set! grob-name (string->symbol (list-ref name-components 1)))
	   (set! context-name (string->symbol (list-ref name-components 0)))))

     (make-music 'ApplyOutputEvent
		 'context-type context-name
		 'procedure
		 (lambda (grob orig-context context)
		   (if (equal?
			(cdr (assoc 'name (ly:grob-property grob 'meta)))
			grob-name)
		       (set! (ly:grob-property grob property) value))))))



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
#(define-music-function (parser location voice-ids music) (list? ly:music?)
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
   (let* ((voices (apply circular-list (make-list (length voice-ids) (list))))
	  (current-voices voices)
	  (current-sequence (list)))
     ;;
     ;; utilities
     (define (push-music m)
       "Push the music expression into the current sequence"
       (set! current-sequence (cons m current-sequence)))
     (define (change-voice)
       "Stores the previously built sequence into the current voice and
       change to the following voice."
       (list-set! current-voices 0 (cons (make-music 'SequentialMusic
						     'elements (reverse! current-sequence))
					 (car current-voices)))
       (set! current-sequence (list))
       (set! current-voices (cdr current-voices)))
     (define (bar-check? m)
       "Checks whether m is a bar check."
       (eq? (ly:music-property m 'name) 'BarCheck))
     (define (music-origin music)
       "Recursively search an origin location stored in music."
       (cond ((null? music) #f)
	     ((not (null? (ly:music-property music 'origin)))
	      (ly:music-property music 'origin))
	     (else (or (music-origin (ly:music-property music 'element))
		       (let ((origins (remove not (map music-origin
						       (ly:music-property music 'elements)))))
			 (and (not (null? origins)) (car origins)))))))
     ;;
     ;; first, split the music and fill in voices
     (map-in-order (lambda (m)
		     (push-music m)
		     (if (bar-check? m) (change-voice)))
		   (ly:music-property music 'elements))
     (if (not (null? current-sequence)) (change-voice))
     ;; un-circularize `voices' and reorder the voices
     (set! voices (map-in-order (lambda (dummy seqs)
				  (reverse! seqs))
				voice-ids voices))
     ;;
     ;; set origin location of each sequence in each voice
     ;; for better type error tracking
     (for-each (lambda (voice)
		 (for-each (lambda (seq)
			     (set! (ly:music-property seq 'origin)
				   (or (music-origin seq) location)))
			   voice))
	       voices)
     ;;
     ;; check sequence length
     (apply for-each (lambda* (#:rest seqs)
			      (let ((moment-reference (ly:music-length (car seqs))))
				(for-each (lambda (seq moment)
					    (if (not (equal? moment moment-reference))
						(ly:music-message seq
								  "Bars in parallel music don't have the same length")))
					  seqs (map-in-order ly:music-length seqs))))
	    voices)
     ;;
     ;; bind voice identifiers to the voices
     (map (lambda (voice-id voice)
	    (ly:parser-define! parser voice-id
			       (make-music 'SequentialMusic
					   'origin location
					   'elements voice)))
	  voice-ids voices))
   ;; Return an empty sequence.  This function is actually a "void" function.
   (make-music 'SequentialMusic 'void #t))

parenthesize =
#(define-music-function (parser loc arg) (ly:music?)
   (_i "Tag @var{arg} to be parenthesized.")

   (if (memq 'event-chord (ly:music-property arg 'types))
       ;; arg is an EventChord -> set the parenthesize property
       ;; on all child notes and rests
       (map
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
                            (list part1 part2)))

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


pitchedTrill =
#(define-music-function
   (parser location main-note secondary-note)
   (ly:music? ly:music?)
   (_i "Print a trill with @var{main-note} as the main note of the trill and
print @var{secondary-note} as a stemless note head in parentheses.")
   (let* ((get-notes (lambda (ev-chord)
                       (filter
                        (lambda (m) (eq? 'NoteEvent (ly:music-property m 'name)))
                        (ly:music-property ev-chord 'elements))))
          (sec-note-events (get-notes secondary-note))
          (trill-events (filter (lambda (m) (music-has-type m 'trill-span-event))
                                (ly:music-property main-note 'elements))))

     (if (pair? sec-note-events)
         (begin
           (let* ((trill-pitch (ly:music-property (car sec-note-events) 'pitch))
                  (forced (ly:music-property (car sec-note-events) 'force-accidental)))

             (if (ly:pitch? trill-pitch)
                 (for-each (lambda (m)
                             (ly:music-set-property! m 'pitch trill-pitch)) trill-events)
                 (begin
                   (ly:warning (_ "Second argument of \\pitchedTrill should be single note: "))
                   (display sec-note-events)))

             (if (eq? forced #t)
                 (for-each (lambda (m)
                             (ly:music-set-property! m 'force-accidental forced))
                           trill-events)))))
     main-note))

quoteDuring =
#(define-music-function (parser location what main-music) (string? ly:music?)
   (_i "Indicate a section of music to be quoted.  @var{what} indicates the name
of the quoted voice, as specified in an @code{\\addQuote} command.
@var{main-music} is used to indicate the length of music to be quoted;
usually contains spacers or multi-measure rests.")
   (make-music 'QuoteMusic
               'element main-music
               'quoted-music-name what))

removeWithTag =
#(define-music-function (parser location tag music) (symbol? ly:music?)
   (_i "Remove elements of @var{music} that are tagged with @var{tag}.")
   (music-filter
    (lambda (m)
      (let* ((tags (ly:music-property m 'tags))
	     (res (memq tag tags)))
	(not res)))
    music))

resetRelativeOctave =
#(define-music-function (parser location reference-note) (ly:music?)
   (_i "Set the octave inside a \\relative section.")

   (let* ((notes (ly:music-property reference-note 'elements))
	  (pitch (ly:music-property (car notes) 'pitch)))

     (set! (ly:music-property reference-note 'elements) '())
     (set! (ly:music-property reference-note 'to-relative-callback)
	   (lambda (music last-pitch)
	     pitch))

     reference-note))

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
#(define-music-function (parser location finger) (number-or-string?)
   (_i "Apply @var{finger} as a fingering indication.")

   (apply make-music
	  (append
	   (list
	    'StrokeFingerEvent
	    'origin location)
	   (if	(string? finger)
		(list 'text finger)
		(list 'digit finger)))))



scaleDurations =
#(define-music-function (parser location fraction music)
   (number-pair? ly:music?)
   (_i "Multiply the duration of events in @var{music} by @var{fraction}.")
   (ly:music-compress music
		      (ly:make-moment (car fraction) (cdr fraction))))

shiftDurations =
#(define-music-function (parser location dur dots arg)
   (integer? integer? ly:music?)
   (_i "Scale @var{arg} up by a factor of @var{2^dur*(2-(1/2)^dots)}.")

   (music-map
    (lambda (x)
      (shift-one-duration-log x dur dots)) arg))

spacingTweaks =
#(define-music-function (parser location parameters) (list?)
   (_i "Set the system stretch, by reading the 'system-stretch property of
the `parameters' assoc list.")
   #{
     \overrideProperty #"Score.NonMusicalPaperColumn"
     #'line-break-system-details
     #$(list (cons 'alignment-extra-space (cdr (assoc 'system-stretch parameters)))
	     (cons 'system-Y-extent (cdr (assoc 'system-Y-extent parameters))))
   #})

styledNoteHeads =
#(define-music-function (parser location style heads music)
   (symbol? list-or-symbol? ly:music?)
   (_i "Set @var{heads} in @var{music} to @var{style}.")
   (style-note-heads heads style music))



tabChordRepetition =
#(define-music-function (parser location) ()
   (_i "Include the string information in a chord repetition.")
   (ly:parser-set-repetition-function parser tab-repeat-chord)
   (make-music 'SequentialMusic 'void #t))

tag =
#(define-music-function (parser location tag arg) (symbol? ly:music?)

   (_i "Add @var{tag} to the @code{tags} property of @var{arg}.")

   (set!
    (ly:music-property arg 'tags)
    (cons tag
	  (ly:music-property arg 'tags)))
   arg)

transposedCueDuring =
#(define-music-function
   (parser location what dir pitch-note main-music)
   (string? ly:dir? ly:music? ly:music?)

   (_i "Insert notes from the part @var{what} into a voice called @code{cue},
using the transposition defined by @var{pitch-note}.  This happens
simultaneously with @var{main-music}, which is usually a rest.	The
argument @var{dir} determines whether the cue notes should be notated
as a first or second voice.")

   (make-music 'QuoteMusic
	       'element main-music
	       'quoted-context-type 'Voice
	       'quoted-context-id "cue"
	       'quoted-music-name what
	       'quoted-voice-direction dir
	       'quoted-transposition (pitch-of-note pitch-note)))

transposition =
#(define-music-function (parser location pitch-note) (ly:music?)
   (_i "Set instrument transposition")

   (context-spec-music
    (make-property-set 'instrumentTransposition
		       (ly:pitch-negate (pitch-of-note pitch-note)))
    'Staff))

tweak =
#(define-music-function (parser location sym val arg)
   (symbol? scheme? ly:music?)
   (_i "Add @code{sym . val} to the @code{tweaks} property of @var{arg}.")

   (if (equal? (object-property sym 'backend-type?) #f)
       (begin
	 (ly:warning (_ "cannot find property type-check for ~a") sym)
	 (ly:warning (_ "doing assignment anyway"))))
   (set!
    (ly:music-property arg 'tweaks)
    (acons sym val
	   (ly:music-property arg 'tweaks)))
   arg)



unfoldRepeats =
#(define-music-function (parser location music) (ly:music?)
   (_i "Force any @code{\\repeat volta}, @code{\\repeat tremolo} or
@code{\\repeat percent} commands in @var{music} to be interpreted
as @code{\\repeat unfold}.")
   (unfold-repeats music))



withMusicProperty =
#(define-music-function (parser location sym val music)
   (symbol? scheme? ly:music?)
   (_i "Set @var{sym} to @var{val} in @var{music}.")

   (set! (ly:music-property music sym) val)
   music)
