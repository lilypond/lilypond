% -*-Scheme-*-

\version "2.12.0"


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% this file is alphabetically sorted.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% need SRFI-1 filter 

#(use-modules (srfi srfi-1))

acciaccatura =
#(def-grace-function startAcciaccaturaMusic stopAcciaccaturaMusic)

addQuote =
#(define-music-function (parser location name music) (string? ly:music?)
   "Add a piece of music to be quoted "
   (add-quotable parser name music)
   (make-music 'SequentialMusic 'void #t))

afterGraceFraction =
#(cons 6 8)

afterGrace =
#(define-music-function
  (parser location main grace)
  (ly:music? ly:music?)

  (let*
      ((main-length (ly:music-length main))
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
				  (cdr fraction)) ))
	(make-music 'GraceMusic
		    'element grace)))))))

applyMusic =
#(define-music-function (parser location func music) (procedure? ly:music?)
               (func music))


applyOutput =
#(define-music-function (parser location ctx proc) (symbol? procedure?)
                (make-music 'ApplyOutputEvent
                  'origin location
                  'procedure proc
                  'context-type ctx))

appoggiatura =
#(def-grace-function startAppoggiaturaMusic stopAppoggiaturaMusic)



% for regression testing purposes.
assertBeamQuant =
#(define-music-function (parser location l r) (pair? pair?)
  (make-grob-property-override 'Beam 'positions
   (ly:make-simple-closure
    (ly:make-simple-closure
     (append
      (list chain-grob-member-functions `(,cons 0 0))
      (check-quant-callbacks l r))))))
    
% for regression testing purposes.
assertBeamSlope =
#(define-music-function (parser location comp) (procedure?)
  (make-grob-property-override 'Beam 'positions
   (ly:make-simple-closure
    (ly:make-simple-closure
     (append
      (list chain-grob-member-functions `(,cons 0 0))
      (check-slope-callbacks comp))))))



autochange =
#(define-music-function (parser location music) (ly:music?)
               (make-autochange-music parser music))

applyContext =
#(define-music-function (parser location proc) (procedure?)
                 (make-music 'ApplyContext 
                   'origin location
                   'procedure proc))


balloonGrobText =
#(define-music-function (parser location grob-name offset text) (symbol? number-pair? markup?)
   
    (make-music 'AnnotateOutputEvent
		'symbol grob-name
		'X-offset (car offset)
		'Y-offset (cdr offset)
		'text text))

balloonText =
#(define-music-function (parser location offset text) (number-pair? markup?)
   
    (make-music 'AnnotateOutputEvent
		'X-offset (car offset)
		'Y-offset (cdr offset)
		'text text))


bar =
#(define-music-function (parser location type)
   (string?)
   (context-spec-music
    (make-property-set 'whichBar type)
    'Timing))


barNumberCheck =
#(define-music-function (parser location n) (integer?)
   (make-music 'ApplyContext 
	       'origin location
	       'procedure 
	       (lambda (c)
		 (let*
		     ((cbn (ly:context-property c 'currentBarNumber)))
		   (if (and  (number? cbn) (not (= cbn n)))
		       (ly:input-message location "Barcheck failed got ~a expect ~a"
					 cbn n))))))


bendAfter =
#(define-music-function (parser location delta) (real?)
	      
  (make-music 'BendAfterEvent
   'delta-step delta))

%% why a function?
breathe =
#(define-music-function (parser location) ()
            (make-music 'EventChord 
              'origin location
              'elements (list (make-music 'BreathingEvent))))


clef =
#(define-music-function (parser location type)
   (string?)
   "Set the current clef."

   (make-clef-set type))


cueDuring = 
#(define-music-function
  (parser location what dir main-music)
  (string? ly:dir? ly:music?)
  (make-music 'QuoteMusic
	      'element main-music 
	      'quoted-context-type 'Voice
	      'quoted-context-id "cue"
	      'quoted-music-name what
	      'quoted-voice-direction dir
	      'origin location))

displayLilyMusic =
#(define-music-function (parser location music) (ly:music?)
   (newline)
   (display-lily-music music parser)
   music)

displayMusic =
#(define-music-function (parser location music) (ly:music?)
   (newline)
   (display-scheme-music music)
   music)


endSpanners =
#(define-music-function (parser location music) (ly:music?)
   (if (eq? (ly:music-property music 'name) 'EventChord)
       (let*
	   ((elts (ly:music-property music 'elements))
	    (start-span-evs (filter (lambda (ev)
				(and (music-has-type ev 'span-event)
				     (equal? (ly:music-property ev 'span-direction)
					     START)))
			      elts))
	    (stop-span-evs
	     (map (lambda (m)
		    (let* ((c (music-clone m)))
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
   "Rearrange durations in ARGUMENT so there is an
acceleration/deceleration. "
   
   (let*
       ((orig-duration (ly:music-length argument))
	(multiplier (ly:make-moment 1 1)))

     (music-map 
      (lambda (mus)
	(if (and (eq? (ly:music-property mus 'name) 'EventChord)
		 (< 0 (ly:moment-main-denominator (ly:music-length mus))))
	    (begin
	      (ly:music-compress mus multiplier)
	      (set! multiplier (ly:moment-mul factor multiplier)))
	    )
	mus)
      argument)

     (ly:music-compress
      argument
      (ly:moment-div orig-duration (ly:music-length argument)))

     argument))

grace =
#(def-grace-function startGraceMusic stopGraceMusic)


"instrument-definitions" = #'()

addInstrumentDefinition =
#(define-music-function
   (parser location name lst) (string? list?)

   (set! instrument-definitions (acons name lst instrument-definitions))

   (make-music 'SequentialMusic 'void #t))


instrumentSwitch =
#(define-music-function
   (parser location name) (string?)
   (let*
       ((handle  (assoc name instrument-definitions))
	(instrument-def (if handle (cdr handle) '()))
	)

     (if (not handle)
	 (ly:input-message "No such instrument: ~a" name))
     (context-spec-music
      (make-music 'SimultaneousMusic
		  'elements
		  (map (lambda (kv)
			 (make-property-set
			  (car kv)
			  (cdr kv)))
		       instrument-def))
      'Staff)))


%% Parser used to read page-layout file, and then retreive score tweaks.
#(define page-layout-parser #f)

includePageLayoutFile = 
#(define-music-function (parser location) ()
   "If page breaks and tweak dump is not asked, and the file
<basename>-page-layout.ly exists, include it."
   (if (not (ly:get-option 'dump-tweaks))
       (let ((tweak-filename (format #f "~a-page-layout.ly"
				     (ly:parser-output-name parser))))
	 (if (access? tweak-filename R_OK)
	     (begin
	       (ly:message "Including tweak file ~a" tweak-filename)
               (set! page-layout-parser (ly:parser-clone parser))
	       (ly:parser-parse-string page-layout-parser
                                       (format #f "\\include \"~a\""
                                               tweak-filename))))))
   (make-music 'SequentialMusic 'void #t))



keepWithTag =
#(define-music-function
  (parser location tag music) (symbol? ly:music?)
  (music-filter
   (lambda (m)
    (let* ((tags (ly:music-property m 'tags))
           (res (memq tag tags)))
     (or
      (eq? tags '())
      res)))
   music))

removeWithTag = 
#(define-music-function
  (parser location tag music) (symbol? ly:music?)
  (music-filter
   (lambda (m)
    (let* ((tags (ly:music-property m 'tags))
           (res (memq tag tags)))
     (not res)))
 music))

killCues =
#(define-music-function
   (parser location music)
   (ly:music?)
   (music-map
    (lambda (mus)
      (if (string? (ly:music-property mus 'quoted-music-name))
	  (ly:music-property mus 'element)
	  mus)) music))

label = 
#(define-music-function (parser location label) (symbol?)
   "Place a bookmarking label, either at top-level or inside music."
   (make-music 'EventChord
	       'page-marker #t
	       'page-label label
	       'elements (list (make-music 'LabelEvent
					   'page-label label)))) 

makeClusters =
#(define-music-function
		(parser location arg) (ly:music?)
		(music-map note-to-cluster arg))

musicMap =
#(define-music-function (parser location proc mus) (procedure? ly:music?)
	     (music-map proc mus))



oldaddlyrics =
#(define-music-function (parser location music lyrics) (ly:music? ly:music?)

              (make-music 'OldLyricCombineMusic 
                          'origin location
                          'elements (list music lyrics)))


overrideProperty =
#(define-music-function (parser location name property value)
   (string? symbol? scheme?)


   "Set @var{property} to @var{value} in all grobs named @var{name}.
The @var{name} argument is a string of the form @code{\"Context.GrobName\"}
or @code{\"GrobName\"}"

   (let*
       ((name-components (string-split name #\.))
	(context-name 'Bottom)
	(grob-name #f))

     (if (> 2 (length name-components))
	 (set! grob-name (string->symbol (car name-components)))
	 (begin
	   (set! grob-name (string->symbol (list-ref name-components 1)))
	   (set! context-name (string->symbol (list-ref name-components 0)))))

     (make-music 'ApplyOutputEvent
		 'origin location
		 'context-type context-name
		 'procedure
		 (lambda (grob orig-context context)
		   (if (equal?
			(cdr (assoc 'name (ly:grob-property grob 'meta)))
			grob-name)
		       (set! (ly:grob-property grob property) value))))))

%% These are music functions (iso music indentifiers), because music identifiers
%% are not allowed at top-level.
pageBreak =
#(define-music-function (location parser) ()
   "Force a page break. May be used at toplevel (ie between scores or
markups), or inside a score."
   (make-music 'EventChord
	       'page-marker #t
	       'line-break-permission 'force
	       'page-break-permission 'force
	       'elements (list (make-music 'LineBreakEvent
					   'break-permission 'force)
			       (make-music 'PageBreakEvent
					   'break-permission 'force))))

noPageBreak =
#(define-music-function (location parser) ()
   "Forbid a page break. May be used at toplevel (ie between scores or
markups), or inside a score."
   (make-music 'EventChord
	       'page-marker #t
	       'page-break-permission 'forbid
	       'elements (list (make-music 'PageBreakEvent
					   'break-permission '()))))

pageTurn =
#(define-music-function (location parser) ()
   "Force a page turn between two scores or top-level markups."
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

noPageTurn =
#(define-music-function (location parser) ()
   "Forbid a page turn. May be used at toplevel (ie between scores or
markups), or inside a score."
   (make-music 'EventChord
	       'page-marker #t
	       'page-turn-permission 'forbid
	       'elements (list (make-music 'PageTurnEvent
					   'break-permission '()))))

allowPageTurn =
#(define-music-function (location parser) ()
   "Allow a page turn. May be used at toplevel (ie between scores or
markups), or inside a score."
   (make-music 'EventChord
	       'page-marker #t
	       'page-turn-permission 'allow
	       'elements (list (make-music 'PageTurnEvent
					   'break-permission 'allow))))

%% Todo:
%% doing
%% define-music-function in a .scm causes crash.

octaveCheck =
#(define-music-function (parser location pitch-note) (ly:music?)
   "octave check"

   (make-music 'RelativeOctaveCheck
	       'origin location
	       'pitch (pitch-of-note pitch-note) 
           ))

ottava = #(define-music-function (parser location octave) (number?)
  "set the octavation "
  (make-ottava-set octave))

partcombine =
#(define-music-function (parser location part1 part2) (ly:music? ly:music?)
                (make-part-combine-music parser
					 (list part1 part2)))

	      
pitchedTrill =
#(define-music-function
   (parser location main-note secondary-note)
   (ly:music? ly:music?)
   (let*
       ((get-notes (lambda (ev-chord)
		     (filter
		      (lambda (m) (eq? 'NoteEvent (ly:music-property m 'name)))
		      (ly:music-property ev-chord 'elements))))
	(sec-note-events (get-notes secondary-note))
	(trill-events (filter (lambda (m) (music-has-type m 'trill-span-event))
			      (ly:music-property main-note 'elements))))

     (if (pair? sec-note-events)
	 (begin
	   (let*
	       ((trill-pitch (ly:music-property (car sec-note-events) 'pitch))
		(forced (ly:music-property (car sec-note-events ) 'force-accidental)))
	     
	     (if (ly:pitch? trill-pitch)
		 (for-each (lambda (m) (ly:music-set-property! m 'pitch trill-pitch))
			   trill-events)
		 (begin
		   (ly:warning (_ "Second argument of \\pitchedTrill should be single note: "))
		   (display sec-note-events)))

	     (if (eq? forced #t)
		 (for-each (lambda (m) (ly:music-set-property! m 'force-accidental forced))
			   trill-events)))))
     main-note))



%% for lambda*
#(use-modules (ice-9 optargs))

parallelMusic =
#(define-music-function (parser location voice-ids music) (list? ly:music?)
  "Define parallel music sequences, separated by '|' (bar check signs),
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
"
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
 ;; Return an empty sequence. this function is actually a "void" function.
 (make-music 'SequentialMusic 'void #t))



parenthesize =
#(define-music-function (parser loc arg) (ly:music?)
   "Tag @var{arg} to be parenthesized."

   (if (memq 'event-chord (ly:music-property arg 'types))
     ; arg is an EventChord -> set the parenthesize property on all child notes and rests
     (map
       (lambda (ev)
         (if (or (memq 'note-event (ly:music-property ev 'types))
                 (memq 'rest-event (ly:music-property ev 'types)))
           (set! (ly:music-property ev 'parenthesize) #t)))
       (ly:music-property arg 'elements))
     ; No chord, simply set property for this expression:
     (set! (ly:music-property arg 'parenthesize) #t))
   arg)


quoteDuring =
#(define-music-function
  (parser location what main-music)
  (string? ly:music?)
  (make-music 'QuoteMusic
	      'element main-music
	      'quoted-music-name what
	      'origin location))



resetRelativeOctave  =
#(define-music-function
    (parser location reference-note)
    (ly:music?)
    "Set the octave inside a \\relative section."

   (let*
    ((notes (ly:music-property reference-note 'elements))
     (pitch (ly:music-property (car notes) 'pitch)))

    (set! (ly:music-property reference-note 'elements) '())
    (set! (ly:music-property reference-note
       'to-relative-callback)
       (lambda (music last-pitch)
        pitch))

    reference-note))


scaleDurations =
#(define-music-function
		  (parser location fraction music) (number-pair? ly:music?)
		  (ly:music-compress music (ly:make-moment (car fraction) (cdr fraction))))



shiftDurations =
#(define-music-function (parser location dur dots arg) (integer? integer? ly:music?)
   ""
   
   (music-map
    (lambda (x)
      (shift-one-duration-log x dur dots)) arg))

spacingTweaks =
#(define-music-function (parser location parameters) (list?)
   "Set the system stretch, by reading the 'system-stretch property of
the `parameters' assoc list."
   #{
      \overrideProperty #"Score.NonMusicalPaperColumn"
        #'line-break-system-details
        #$(list (cons 'alignment-extra-space (cdr (assoc 'system-stretch parameters)))
		(cons 'system-Y-extent (cdr (assoc 'system-Y-extent parameters))))
   #})


rightHandFinger =
#(define-music-function (parser location finger) (number-or-string?)
   "Define a StrokeFingerEvent"
   
   (apply make-music
	  (append
	   (list 
	    'StrokeFingerEvent
	    'origin location)
	   (if  (string? finger)
		(list 'text finger)
		(list 'digit finger)))))

scoreTweak =
#(define-music-function (parser location name) (string?)
   "Include the score tweak, if exists."
   (if (and page-layout-parser (not (ly:get-option 'dump-tweaks)))
       (let ((tweak-music (ly:parser-lookup page-layout-parser
                                            (string->symbol name))))
         (if (ly:music? tweak-music)
             tweak-music
             (make-music 'SequentialMusic)))
       (make-music 'SequentialMusic)))


tag = #(define-music-function (parser location tag arg)
   (symbol? ly:music?)

   "Add @var{tag} to the @code{tags} property of @var{arg}."

   (set!
    (ly:music-property arg 'tags)
    (cons tag
	  (ly:music-property arg 'tags)))
   arg)



transposedCueDuring =
#(define-music-function
  (parser location what dir pitch-note main-music)
  (string? ly:dir? ly:music? ly:music?)

  "Insert notes from the part @var{what} into a voice called @code{cue},
using the transposition defined by @var{pitch-note}.  This happens
simultaneously with @var{main-music}, which is usually a rest.  The
argument @var{dir} determines whether the cue notes should be notated
as a first or second voice."

  (make-music 'QuoteMusic
	      'element main-music
	      'quoted-context-type 'Voice
	      'quoted-context-id "cue"
	      'quoted-music-name what
	      'quoted-voice-direction dir
	      'quoted-transposition (pitch-of-note pitch-note)
	      'origin location))



transposition =
#(define-music-function (parser location pitch-note) (ly:music?)
   "Set instrument transposition"

   (context-spec-music
    (make-property-set 'instrumentTransposition
		       (ly:pitch-negate (pitch-of-note pitch-note)))
        'Staff))

tweak = #(define-music-function (parser location sym val arg)
	   (symbol? scheme? ly:music?)

	   "Add @code{sym . val} to the @code{tweaks} property of @var{arg}."
	   
	   (set!
	    (ly:music-property arg 'tweaks)
	    (acons sym val
		   (ly:music-property arg 'tweaks)))
	   arg)



unfoldRepeats =
#(define-music-function (parser location music) (ly:music?)
		  (unfold-repeats music))



withMusicProperty =
#(define-music-function (parser location sym val music) (symbol? scheme? ly:music?)
   "Set @var{sym} to @var{val} in @var{music}."

   (set! (ly:music-property music sym) val)
   music)
