\version "2.3.2"


applymusic = #(def-music-function (location func music) (procedure? ly:music?)
               (func music))

addlyrics = #(def-music-function (location music lyrics) (ly:music? ly:music?)
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

%% \mytag #'foo { ... } ==> OK
%% c-\mytag #'foo ^4    ==> KO
%{
#(use-modules (srfi srfi-1))
#(define-public (symbol-or-symbols? obj)
  "Return #t iif obj is a symbol or a symbol list."
  (or (symbol? obj)
      (and (list? obj)
           (null? (remove symbol? obj)))))

mytag = #(def-music-function (location tagname music) (symbol-or-symbols? ly:music?)
        (set! (ly:music-property music 'tags)
              ((if (list? tagname) append cons) tagname (ly:music-property music 'tags)))
        music)
%}

%{

TODO:

remove these from the parser, and softcode here:

 * \tag

with small syntax changes, we could also do

 * \bar
 *  ?

%}
