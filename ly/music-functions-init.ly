\version "2.3.16"


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

%{

TODO:

remove these from the parser, and softcode here:

 * \tag

with small syntax changes, we could also do

 * \bar
 *  ?

%}
