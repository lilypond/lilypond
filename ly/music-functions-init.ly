% -*-Scheme-*-

\version "2.7.32"

%% need SRFI-1 filter 

#(use-modules (srfi srfi-1))  


tweak = #(def-music-function (parser location sym val arg)
	   (symbol? scheme? ly:music?)

	   "Add @code{sym . val} to the @code{tweaks} property of @var{arg}."

	   
	   (set!
	    (ly:music-property arg 'tweaks)
	    (acons sym val
		   (ly:music-property arg 'tweaks)))
	   arg)

tag = #(def-music-function (parser location tag arg)
   (symbol? ly:music?)

   "Add @var{tag} to the @code{tags} property of @var{arg}."

   (set!
    (ly:music-property arg 'tags)
    (cons tag
	  (ly:music-property arg 'tags)))
   arg)

clef =
#(def-music-function (parser location type)
   (string?)
   
   "Set the current clef."

   (make-clef-set type))

bar =
#(def-music-function (parser location type)
   (string?)
   (context-spec-music
    (make-property-set 'whichBar type)
    'Timing))

applyMusic =
#(def-music-function (parser location func music) (procedure? ly:music?)
               (func music))

oldaddlyrics =
#(def-music-function (parser location music lyrics) (ly:music? ly:music?)

              (make-music 'OldLyricCombineMusic 
                          'origin location
                          'elements (list music lyrics)))

grace =
#(def-grace-function startGraceMusic stopGraceMusic)

acciaccatura =
#(def-grace-function startAcciaccaturaMusic stopAcciaccaturaMusic)
appoggiatura =
#(def-grace-function startAppoggiaturaMusic stopAppoggiaturaMusic)

partcombine =
#(def-music-function (parser location part1 part2) (ly:music? ly:music?)
                (make-part-combine-music (list part1 part2)))

autochange =
#(def-music-function (parser location music) (ly:music?)
               (make-autochange-music music))

applyContext =
#(def-music-function (parser location proc) (procedure?)
                 (make-music 'ApplyContext 
                   'origin location
                   'procedure proc))

musicMap =
#(def-music-function (parser location proc mus) (procedure? ly:music?)
	     (music-map proc mus))

displayMusic =
#(def-music-function (parser location music) (ly:music?)
		 (display-scheme-music music)
		 music)

%% FIXME: guile-1.7 required?
%#(use-modules (scm display-lily))invalid module name for use-syntax ((srfi srfi-39))

#(use-modules (scm display-lily))
#(display-lily-init parser)
displayLilyMusic =
#(def-music-function (parser location music) (ly:music?)
   (display-lily-music music)
   music)

applyOutput =
#(def-music-function (parser location proc) (procedure?)
                (make-music 'ApplyOutputEvent 
                  'origin location
                  'procedure proc))

overrideProperty =
#(def-music-function (parser location name property value)
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

     (context-spec-music
      (make-music 'ApplyOutputEvent
		  'origin location
		  'procedure
		  (lambda (grob orig-context context)
		    (if (equal?
			 (cdr (assoc 'name (ly:grob-property grob 'meta)))
			 grob-name)
			(set! (ly:grob-property grob property) value)
			)))

      context-name)))

breathe =
#(def-music-function (parser location) ()
            (make-music 'EventChord 
              'origin location
              'elements (list (make-music 'BreathingSignEvent))))


unfoldRepeats =
#(def-music-function (parser location music) (ly:music?)
		  (unfold-repeats music))

compressMusic =
#(def-music-function
		  (parser location fraction music) (number-pair? ly:music?)
		  (ly:music-compress music (ly:make-moment (car fraction) (cdr fraction))))

makeClusters =
#(def-music-function
		(parser location arg) (ly:music?)
		(music-map note-to-cluster arg))


removeWithTag = 
#(def-music-function
  (parser location tag music) (symbol? ly:music?)
  (music-filter
   (lambda (m)
    (let* ((tags (ly:music-property m 'tags))
           (res (memq tag tags)))
     (not res)))
 music))
	      
keepWithTag =
#(def-music-function
  (parser location tag music) (symbol? ly:music?)
  (music-filter
   (lambda (m)
    (let* ((tags (ly:music-property m 'tags))
           (res (memq tag tags)))
     (or
      (eq? tags '())
      res)))
   music))


%% Todo:
%% doing
%% def-music-function in a .scm causes crash.

cueDuring = 
#(def-music-function
  (parser location what dir main-music)
  (string? ly:dir? ly:music?)
  (make-music 'QuoteMusic
	      'element main-music 
	      'quoted-context-type 'Voice
	      'quoted-context-id "cue"
	      'quoted-music-name what
	      'quoted-voice-direction dir
	      'origin location))


quoteDuring = #
(def-music-function
  (parser location what main-music)
  (string? ly:music?)
  (make-music 'QuoteMusic
	      'element main-music
	      'quoted-music-name what
	      'origin location))



pitchedTrill =
#(def-music-function
   (parser location main-note secondary-note)
   (ly:music? ly:music?)
   (let*
       ((get-notes (lambda (ev-chord)
		     (filter
		      (lambda (m) (eq? 'NoteEvent (ly:music-property m 'name)))
		      (ly:music-property ev-chord 'elements))))
	(sec-note-events (get-notes secondary-note))
	(trill-events (filter (lambda (m) (memq 'trill-span-event (ly:music-property m 'types)))
			      (ly:music-property main-note 'elements)))

	(trill-pitch
	 (if (pair? sec-note-events)
	     (ly:music-property (car sec-note-events) 'pitch)
	     )))
     
     (if (ly:pitch? trill-pitch)
	 (for-each (lambda (m) (ly:music-set-property! m 'pitch trill-pitch))
		   trill-events)
	 (begin
	   (ly:warning (_ "Second argument of \\pitchedTrill should be single note: "))
	   (display sec-note-events)))

     main-note))

killCues =
#(def-music-function
   (parser location music)
   (ly:music?)
   (music-map
    (lambda (mus)
      (if (string? (ly:music-property mus 'quoted-music-name))
	  (ly:music-property mus 'element)
	  mus)) music))
   

afterGraceFraction =
#(cons 6 8)

afterGrace =
#(def-music-function
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


barNumberCheck =
#(def-music-function (parser location n) (integer?)
   (make-music 'ApplyContext 
	       'origin location
	       'procedure 
	       (lambda (c)
		 (let*
		     ((cbn (ly:context-property c 'currentBarNumber)))
		   (if (not (= cbn n))
		       (ly:input-message location "Barcheck failed got ~a expect ~a"
					 cbn n))))))



% for regression testing purposes.
assertBeamQuant =
#(def-music-function (parser location l r) (pair? pair?)
  (make-grob-property-override 'Beam 'positions
   (ly:make-simple-closure
    (ly:make-simple-closure
     (append
      (list chain-grob-member-functions `(,cons 0 0))
      (check-quant-callbacks l r))))))
    
% for regression testing purposes.
assertBeamSlope =
#(def-music-function (parser location comp) (procedure?)
  (make-grob-property-override 'Beam 'positions
   (ly:make-simple-closure
    (ly:make-simple-closure
     (append
      (list chain-grob-member-functions `(,cons 0 0))
      (check-slope-callbacks comp))))))


parallelMusic =
#(def-music-function (parser location voice-ids music) (list? ly:music?)
  "Define parallel music sequences, separated by '|' (bar check signs),
and assign them to the identifiers provided in @var{voice-ids}.

@var{voice-ids}: a list of music identifiers (symbols containing only letters)

@var{music}: a music sequence, containing BarChecks as limiting expressions.

Example:
  \\parallelMusic #'(A B C) {
    c c | d d | e e |
    d d | e e | f f |
  }
<==>
  A = { c c | d d | }
  B = { d d | e e | }
  C = { e e | f f | }
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
    (apply for-each (lambda (. seqs)
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




%% this is a stub. Write your own to suit the spacing tweak output.
spacingTweaks =
#(def-music-function (parser location parameters) (list?)
   (make-music 'SequentialMusic 'void #t))

octave =
#(def-music-function (parser location pitch-note) (ly:music?)
   "octave check"

   (make-music 'RelativeOctaveCheck
	       'origin location
	       'pitch (pitch-of-note pitch-note) 
	       ))

addquote =
#(def-music-function (parser location name music) (string? ly:music?)
   "Add a piece of music to be quoted "
   (add-quotable name music)
   (make-music 'SequentialMusic 'void #t))

   
