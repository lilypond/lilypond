% -*-Scheme-*-

\version "2.6.0"

%% need SRFI-1 filter 

#(use-modules (srfi srfi-1))  

applymusic =
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

applycontext =
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

#(use-modules (scm display-lily))
displayLilyMusic =
#(def-music-function (parser location music) (ly:music?)
   (display-lily-init parser)
   (display-lily-music music)
   music)

applyoutput =
#(def-music-function (parser location proc) (procedure?)
                (make-music 'ApplyOutputEvent 
                  'origin location
                  'procedure proc))

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
	 (for-each (lambda (m) (ly:music-set-property! m 'trill-pitch trill-pitch))
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


%{

TODO:

remove these from the parser, and softcode here:

 * \tag

with small syntax changes, we could also do

 * \bar
 *  ?

%}

