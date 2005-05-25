\version "2.4.0"


applymusic = #(def-music-function (parser location func music) (procedure? ly:music?)
               (func music))

oldaddlyrics = #(def-music-function (parser location music lyrics) (ly:music? ly:music?)

              (make-music 'OldLyricCombineMusic 
                          'origin location
                          'elements (list music lyrics)))

grace = #(def-grace-function startGraceMusic stopGraceMusic)

acciaccatura = #(def-grace-function startAcciaccaturaMusic stopAcciaccaturaMusic)
appoggiatura = #(def-grace-function startAppoggiaturaMusic stopAppoggiaturaMusic)

partcombine = #(def-music-function (parser location part1 part2) (ly:music? ly:music?)
                (make-part-combine-music (list part1 part2)))

autochange = #(def-music-function (parser location music) (ly:music?)
               (make-autochange-music music))

applycontext = #(def-music-function (parser location proc) (procedure?)
                 (make-music 'ApplyContext 
                   'origin location
                   'procedure proc))

displayMusic = #(def-music-function (parser location music) (ly:music?)
		 (display-scheme-music music)
		 music)
applyoutput = #(def-music-function (parser location proc) (procedure?)
                (make-music 'ApplyOutputEvent 
                  'origin location
                  'procedure proc))

breathe = #(def-music-function (parser location) ()
            (make-music 'EventChord 
              'origin location
              'elements (list (make-music 'BreathingSignEvent))))


unfoldrepeats = #(def-music-function (parser location music) (ly:music?)
		  (unfold-repeats music))

compressmusic = #(def-music-function
		  (parser location fraction music) (number-pair? ly:music?)
		  (ly:music-compress music (ly:make-moment (car fraction) (cdr fraction))))

makeClusters = #(def-music-function
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




killCues =
#(def-music-function
   (parser location music)
   (ly:music?)
   (music-map
    (lambda (mus)
      (if (string? (ly:music-property mus 'quoted-music-name))
	  (ly:music-property mus 'element)
	  mus)) music))
   

afterGraceFraction = #(cons 6 8)

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

%{

TODO:

remove these from the parser, and softcode here:

 * \tag

with small syntax changes, we could also do

 * \bar
 *  ?

%}
