\version "2.4.0"


applymusic = #(def-music-function (location func music) (procedure? ly:music?)
               (func music))

oldaddlyrics = #(def-music-function (location music lyrics) (ly:music? ly:music?)

              (make-music 'LyricCombineMusic 
                          'origin location
                          'elements (list music lyrics)))

grace = #(def-grace-function startGraceMusic stopGraceMusic)
acciaccatura = #(def-grace-function startAcciaccaturaMusic stopAcciaccaturaMusic)
appoggiatura = #(def-grace-function startAppoggiaturaMusic stopAppoggiaturaMusic)

partcombine = #(def-music-function (location part1 part2) (ly:music? ly:music?)
                (make-part-combine-music (list part1 part2)))

autochange = #(def-music-function (location music) (ly:music?)
               (make-autochange-music music))

applycontext = #(def-music-function (location proc) (procedure?)
                 (make-music 'ApplyContext 
                   'origin location
                   'procedure proc))

displayMusic = #(def-music-function (location music) (ly:music?)
		 (display-music music)
		 music)
applyoutput = #(def-music-function (location proc) (procedure?)
                (make-music 'ApplyOutputEvent 
                  'origin location
                  'procedure proc))

breathe = #(def-music-function (location) ()
            (make-music 'EventChord 
              'origin location
              'elements (list (make-music 'BreathingSignEvent))))


unfoldrepeats = #(def-music-function (location music) (ly:music?)
		  (unfold-repeats music))

compressmusic = #(def-music-function
		  (location fraction music) (number-pair? ly:music?)
		  (ly:music-compress music (ly:make-moment (car fraction) (cdr fraction))))

makeClusters = #(def-music-function
		(location arg) (ly:music?)
		(music-map note-to-cluster arg))


removeWithTag = 
#(def-music-function
  (location tag music) (symbol? ly:music?)
  (music-filter
   (lambda (m)
    (let* ((tags (ly:music-property m 'tags))
	    (res (memq tag tags)))
       (not res)))))
	      
keepWithTag =
#(def-music-function
  (location tag music) (symbol? ly:music?)
  (music-filter
   (lambda (m)
    (let* ((tags (ly:music-property m 'tags))
	   (res (memq tag tags)))
       (or
	(eq? tags '())
	(memq tag tags))))
   music))


%% Todo:
%% doing
%% def-music-function in a .scm causes crash.

quoteDuring = #
(def-music-function
  (location what dir main-music)
  (string? ly:dir? ly:music?)
  (let*
      ((quote-music
	(make-music 'NewQuoteMusic
		    'quoted-context-type 'Voice
		    'quoted-context-id "cue"
		    'quoted-music-name what
		    'origin location))
       (main-voice (if (= 1 dir) 2 1))
       (cue-voice (if (= 1 dir) 1 2))
       (return-value quote-music)
       )

    (if (not (= dir 0))
	(begin
	  (set! return-value
		(make-sequential-music
		 (list
		  (context-spec-music (make-voice-props-set cue-voice) 'Voice "cue")
		  quote-music
		  (context-spec-music (make-voice-props-revert)  'Voice "cue"))
		 ))

	  (set! main-music
		(make-sequential-music
		 (list
		  (make-voice-props-set main-voice)
		  main-music
		  (make-voice-props-revert)))
		)))
    (set! (ly:music-property quote-music 'element) main-music)
    return-value))

%{

TODO:

remove these from the parser, and softcode here:

 * \tag

with small syntax changes, we could also do

 * \bar
 *  ?

%}
