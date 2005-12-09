% -*-Scheme-*-

\version "2.6.0"

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

tag = #(def-music-function (parser location tag arg)
   (symbol? ly:music?)

   "Add @var{tag} to the @code{tags} property of @var{arg}."

   (set!
    (ly:music-property arg 'tags)
    (cons tag
	  (ly:music-property arg 'tags)))
   arg)

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
    



%{

TODO:

remove these from the parser, and softcode here:

 * \tag

with small syntax changes, we could also do

 * \bar
 *  ?

%}

