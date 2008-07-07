\version "2.11.51"
#(use-modules (srfi srfi-13)
              (ice-9 format))

%%%
%%% Testing utilities
%%%
#(use-modules (scm display-lily))
#(memoize-clef-names supported-clefs)
#(define (parse-lily-and-compute-lily-string chr port)
  (let ((lily-string (call-with-output-string
                      (lambda (out)
                        (do ((c (read-char port) (read-char port)))
                            ((and (char=? c #\#)
                                  (char=? (peek-char port) #\]))
                             (read-char port))
                          (display c out))))))
    `(let* ((parser-clone (ly:parser-clone parser))
            (input-str (string-trim-both ,lily-string))
            (music (car (ly:music-property (parse-string-result input-str
                                                                parser-clone)
                                           'elements)))
            (result-str (string-trim-both (music->lily-string music parser-clone))))
       (cons input-str result-str))))

#(read-hash-extend #\[ parse-lily-and-compute-lily-string) %{ ] %}

#(define (lily-string->markup str)
   (make-column-markup (string-split str #\NewLine)))

#(define test-function
  (let ((test-number 0))
   (lambda (parser location result-info strings)
    (let ((input (car strings))
         (output (cdr strings)))
     (set! test-number (1+ test-number))
     (if (not (equal? input output))
         (ly:progress "Test ~a unequal: ~a. \nin  = ~a\nout = ~a\n"
	  test-number
	  (if result-info
	      result-info "BUG")
          input output))
     (make-music 'SequentialMusic 'void #t)))))
	  
test = 
#(define-music-function (parser location result-info strings) (string? pair?)
   (test-function parser location result-info strings))

%%%
%%% Tests
%%%
\header {
  texidoc = "This is a test of the display-lily-music unit. Problems are reported on the
stderr of this run." 
}

%% Sequential music
\test "" ##[ { { a b } { c d } } #]		% SequentialMusic
\test "" ##[ << { a b } { c d } >> #]		% SimultaneousMusic
\test "" ##[ << { a b } \\ { c d } >> #]	% VoiceSeparator

%% Chords and Notes
\test "" ##[ { ceses ces c cis cisis } #]	% NoteEvent
\test "" ##[ { deses des d dis disis } #]
\test "" ##[ { eeses ees e eis eisis } #]
\test "" ##[ { feses fes f fis fisis } #]
\test "" ##[ { geses ges g gis gisis } #]
\test "" ##[ { aeses aes a ais aisis } #]
\test "" ##[ { beses bes b bis bisis } #]
\test "" ##[ { c,, d' } #]
\test "" ##[ { c' d'=' } #]
\test "" ##[ { c! c? } #]
\test "" ##[ r1.*4/3 #]		% RestEvent
\test "" ##[ c1\rest #]		% RestEvent
\test "" ##[ s2..*3/4 #]		% SkipEvent
\test "" ##[ R1.*2/3 #]		% MultiMeasureRestMusicGroup, MultiMeasureRestEvent
\test "" ##[ \skip 2.*3/4 #]		% SkipMusic
\test "" ##[ < c\1 e\3 >4.*3/4-. #]	% EventChord, NoteEvent, StringNumberEvent, ArticulationEvent

%% tags
\test "" ##[ { \tag #'foo { c4 d } } #]
\test "" ##[ c-\tag #'foo -\tag #'baz -^ -. #]

%% Graces
\test "" ##[ { \grace c8 d2 } #]			% GraceMusic
\test "" ##[ { \appoggiatura c8 d2 } #]
\test "" ##[ { \acciaccatura c8 d2 } #]
\test "" ##[ { c1 \afterGrace { b,16 c } d2 } #]

%% Clusters
\test "" ##[ { \makeClusters { c4 g } } #]		% ClusterNoteEvent

%% Figured bass
\test "" ##[ \figures { < 6 > } #]			% BassFigureEvent
\test "" ##[ \figuremode { < 1-- 3- > < 2+ 4++ > < _! 7! > } #]
\test "" ##[ \figuremode { < [6 > < 5] > } #]

%% Lyrics
\test "" ##[ \lyrics { a b } #]
\test "" ##[ \lyricmode { a --  b } #] 		% HyphenEvent
\test "" ##[ \lyricmode { a __  b } #] 		% ExtenderEvent
\test "" ##[ \lyricmode { "a " } #] 			% LyricEvent
\test "" ##[ \lyricsto "foo" { bla bla } #]		% LyricCombineMusic
\test "" ##[ { { c d }
  \addlyrics { bla bla } } #]

%% Drums
\test "" ##[ \drums { hihat } #]
\test "" ##[ \drummode { hihat4.*3/4 } #]

%% Expressive marks
\test "" ##[ c4 ~ #]			 		% TieEvent
\test "" ##[ c\noBeam #] 				% BeamForbidEvent
\test "" ##[ c\1 #] 					% StringNumberEvent
\test "" ##[ { c:  c:1  } #]				% TremoloEvent
\test "" ##[ { c-^  c^^  c_^  } #]			% ArticulationEvent
\test "" ##[ { c-+  c^+  c_+  } #]
\test "" ##[ { c--  c^-  c_-  } #]
\test "" ##[ { c-|  c^|  c_|  } #]
\test "" ##[ { c->  c^>  c_>  } #]
\test "" ##[ { c-.  c^.  c_.  } #]
\test "" ##[ { c-_  c^_  c__  } #]
\test "" ##[ { c-\trill  c^\trill  c_\trill  } #]
\test "" ##[ { c-1  c^2  c_3  } #]			% FingerEvent
\test "" ##[ { c-"foo"  c^"foo"  c_"foo"  } #]	% TextScriptEvent
\test "" ##[ { R1*4-"foo"  R^"foo"  R_"foo"  } #]	% MultiMeasureTextEvent
\test "" ##[ { c4-\harmonic  c^\harmonic  c_\harmonic  } #] 	% HarmonicEvent
\test "" ##[ { c-\glissando  c^\glissando  c_\glissando  } #] % GlissandoEvent
\test "" ##[ { c-\arpeggio  c^\arpeggio  c_\arpeggio  } #] 	% ArpeggioEvent
\test "" ##[ { c\p  c^\ff  c_\sfz  } #] 		% AbsoluteDynamicEvent
\test "" ##[ { c[  c]  c^[  c^]  c_[  c_]  } #] 	% BeamEvent
\test "" ##[ { c(  c)  c^(  c^)  c_(  c_)  } #] 	% SlurEvent
\test "" ##[ { c\<  c\!  c^\<  c^\!  c_\<  c_\!  } #]	% CrescendoEvent
\test "" ##[ { c\>  c\!  c^\>  c^\!  c_\>  c_\!  } #]	% DecrescendoEvent
\test "" ##[ { c\(  c\)  c^\(  c^\)  c_\(  c_\)  } #]	% PhrasingSlurEvent
\test "" ##[ { c\sustainOn  c\sustainOff  } #] % SustainEvent
\test "" ##[ { c\sostenutoOn  c\sostenutoOff  } #] % SostenutoEvent
\test "" ##[ \melisma #]
\test "" ##[ \melismaEnd #]
\test "" ##[ { c\startTextSpan  c\stopTextSpan  } #] % TextSpanEvent
\test "" ##[ { c\startTrillSpan  c\stopTrillSpan  } #] % TrillSpanEvent
\test "" ##[ { c \startStaff c \stopStaff } #]	% StaffSpanEvent
\test "" ##[ { c\startGroup  c\stopGroup  c^\startGroup  c^\stopGroup  c_\startGroup  c_\stopGroup  } #] % NoteGroupingEvent
\test "" ##[ { c\unaCorda  c\treCorde  } #] % UnaCordaEvent
\test "" ##[ \breathe #]
\test "" ##[ { c \[ c \] } #]			% LigatureEvent
\test "" ##[ \~ #]					% PesOrFlexaEvent

\test "" ##[ \break #]
\test "" ##[ \noBreak #]
\test "" ##[ \pageBreak #]
\test "" ##[ \noPageBreak #]
\test "" ##[ \pageTurn #]
\test "" ##[ \noPageTurn #]

%% Checks
\test "" ##[ \octave a' #]				% RelativeOctaveCheck
\test "" ##[ | #]					% BarCheck

%% Marks
\test "" ##[ \mark \default #]			% MarkEvent
\test "" ##[ \mark "Allegro" #]
\test "" ##[ \tempo 4 = 120 #]			% MetronomeChangeEvent

%% key, time, clef, bar
\test "" ##[ \key \default #]			% KeyChangeEvent
\test "" ##[ \key e \minor #]
\test "" ##[ \clef "bass" #]
\test "" ##[ \clef "french^2" #]
\test "" ##[ \clef "alto_3" #]
\test "" ##[ \time 2/4 #]
\test "" ##[ #(set-time-signature 5 8 '(3 2)) #]
\test "" ##[ \bar "|." #]

%% staff switches
\test "" ##[ \autochange { c d } #]			% AutoChangeMusic
\test "" ##[ { \change Staff = "up" { c d } } #]	% ContextChange

%% Tuplets
\test "" ##[ \times 2/3 { c8 d e } #]				% TimeScaledMusic
\test "" ##[ \times 4/6 { c16 d e f g a } #]
%}
%% \relative and \tranpose
\test #"NOT A BUG" ##[ \relative c' { c b } #]	% RelativeOctaveMusic
\test #"NOT A BUG" ##[ \transpose c d { c d } #]	% TransposedMusic
%}
%% Repeats
\test "" ##[ \repeat volta 2 { c d } #]		% VoltaRepeatedMusic
\test "" ##[ \repeat unfold 2 { c d } #]		% UnfoldedRepeatedMusic
\test "" ##[ \repeat percent 2 { c d } #]		% PercentRepeatedMusic
\test "" ##[ \repeat tremolo 4 { c16 d } #]		% TremoloRepeatedMusic
\test "" ##[ \repeat volta 2 { c4 d } \alternative { { c d } { e f } } #] % 

%% Context creation
\test "" ##[ \new Staff { c d } #]			% ContextSpeccedMusic
\test "" ##[ \new Staff = "up" { c d } #]			% ContextSpeccedMusic
\test "" ##[ \context Staff { c d } #]
\test "" ##[ \context Staff = "up" { c d } #]
\test "" ##[
\new Staff \with {
  \consists "Timing_engraver"
  \remove "Clef_engraver"
} { c d } #]
%% Context properties
\test "" ##[ \once \set Score . skipBars = ##t #]	% PropertySet
\test "" ##[ \set autoBeaming = ##f #]
\test "" ##[ \unset Score . skipBars #]		% PropertyUnset
\test "" ##[ \unset autoBeaming #]
%% Layout properties
\test "" ##[ \override Staff . Stem #'thickness = #4.0 #]	% OverrideProperty
\test "" ##[ \once \override Beam #'thickness = #0.6 #]
\test "" ##[ \revert Staff . Stem #'thickness #]	% RevertProperty
\test "" ##[ \revert Beam #'thickness #]

%% \applyOutput
\test "" ##[ \applyOutput #'Foo #(lambda (arg) (list)) #]
%% \applyContext
\test "" ##[ \applyContext #(lambda (arg) (list)) #]

%% \partial
\test "" ##[ \partial 2 #]
\test "" ##[ \partial 8. #]
\test #"TODO? exotic durations in \\partial" ##[ \partial 4*2/3 #]

%% \partcombine
\test "" ##[ \partcombine { c e }
{ d f } #]						% PartCombineMusic UnrelativableMusic

%% Cue notes
\test "" ##[ \cueDuring #"foo" #1 { c d } #]
\test "" ##[ \quoteDuring #"foo" { c d } #]


%% end test.

#(read-hash-extend #\[ #f) %{ ] %}
