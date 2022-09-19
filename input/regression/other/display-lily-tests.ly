\version "2.23.14"

#(use-modules (srfi srfi-13)
              (ice-9 format)
              (ice-9 match))

%%%
%%% Testing utilities
%%%
#(use-modules (lily display-lily))
#(memoize-clef-names supported-clefs)

%% Warn about untested music types. This does not guarantee 100% coverage
%% (the form of the lily string might need to depend on the music property
%% values), but does catch trivial cases of forgetting to add a display method
%% for a new event.
#(define tested-music-types (make-hash-table))

%% Some events are sent internally but never present in the user-written
%% input music.  Such events can be excluded from the check using
%% \ignoreMusicType.
ignoreMusicType =
#(define-void-function (type) (symbol?)
   (hashq-set! tested-music-types type #t))

#(define (parse-lily-and-compute-lily-string chr port)
  (let ((lily-string (call-with-output-string
                      (lambda (out)
                        (do ((c (read-char port) (read-char port)))
                            ((and (char=? c #\#)
                                  (char=? (peek-char port) #\]))
                             (read-char port))
                          (display c out))))))
    `(let* ((parser-clone (ly:parser-clone))
            (input-str (string-trim-both ,lily-string))
            (music (ly:parse-string-expression parser-clone input-str))
            (result-str (string-trim-both (music->lily-string music))))
       (for-some-music
        (lambda (m)
          (ignoreMusicType (ly:music-property m 'name))
          #f)
        music)
       (cons input-str result-str))))

#(read-hash-extend #\[ parse-lily-and-compute-lily-string) %{ ] %}
#(ly:set-option 'warning-as-error #t)

%% option can be one of the following
%%
%% not provided    The expected output is the input.  This should be
%%                 the norm.  Any difference will be logged and cause
%%                 the test to fail.
%%
%% "NOT A BUG"     The expected output differs from the input in an
%%                 unspecified way.  The difference will be logged
%%                 but will not cause the test to fail.
%%
%% "BUG"           The expected output differs from the input in an
%%                 unspecified way.  The difference will be logged
%%                 but will not cause the test to fail.
%%
%% other           This is the exact expected output.  Any difference
%%                 will be logged and cause the test to fail.
test =
#(define-void-function (option strings)
  ((string?) pair?)
  (let* ((input (car strings))
         (actual-output (cdr strings))
         (suppression (cond
                       ((equal? option "BUG") option)
                       ((equal? option "NOT A BUG") option)
                       (else #f)))
         (expected-output (if (or (not option) suppression)
                           input
                           option)))
   (if (not (equal? expected-output actual-output))
    (if suppression
     (ly:progress "Test unequal: ~a.
input    = ~a
expected = ~a
actual   = ~a"
      suppression input expected-output actual-output)
     (ly:input-warning (*location*) "Test unequal: BUG.
input    = ~a
expected = ~a
actual   = ~a"
      input expected-output actual-output)))))

%%%
%%% Tests
%%%
\header {
  texidoc = "This is a test of the display-lily-music unit. Problems
are reported on the stderr of this run."
}

%% Void music
\test "##{#}" ##[ #]
\test ##[ ##{#} #]

%% Sequential music
\test ##[ { { a4 b4 } { c4 d4 } } #]		% SequentialMusic
\test ##[ << { a4 b4 } { c4 d4 } >> #]		% SimultaneousMusic
\test ##[ << { a4 b4 } \\ { c4 d4 } >> #]	% VoiceSeparator

%% Chords and Notes
\test ##[ { ceses4 ces4 c4 cis4 cisis4 } #]	% NoteEvent
\test ##[ { deses4 des4 d4 dis4 disis4 } #]
\test ##[ { eeses4 ees4 e4 eis4 eisis4 } #]
\test ##[ { feses4 fes4 f4 fis4 fisis4 } #]
\test ##[ { geses4 ges4 g4 gis4 gisis4 } #]
\test ##[ { aeses4 aes4 a4 ais4 aisis4 } #]
\test ##[ { beses4 bes4 b4 bis4 bisis4 } #]
\test ##[ { c,,4 d'4 } #]
\test ##[ { c'4 d'='4 } #]
\test ##[ { c!4 c?4 } #]
\test ##[ r1.*4/3 #]		% RestEvent
\test ##[ c1\rest #]		% RestEvent
\test ##[ s2..*3/4 #]		% SkipEvent
\test ##[ R1.*2/3 #]		% MultiMeasureRestMusicGroup, MultiMeasureRestEvent
\test ##[ \skip 2.*3/4 #]		% SkipMusic
\test ##[ \skip { c4 } #]		% SkippedMusic
\test ##[ < c\1 e\3 >4.*3/4-. #]	% EventChord, NoteEvent, StringNumberEvent, ArticulationEvent
\test ##[ < c-1\4 >8 #]
\test ##[ { < c e g c' >4 q8-. } #] % RepeatedChord

%% tags
\test ##[ { \tag #'foo { c4 d4 } } #]
\test ##[ c4-\tag #'foo -\tag #'baz -^-. #]

%% Graces
\test ##[ { \grace c8 d2 } #]				% GraceMusic
\test ##[ { \appoggiatura c8 d2 } #]
\test ##[ { \acciaccatura c8 d2 } #]
\test ##[ { c1 \afterGrace 3/4 { b,16 c16 } d2 } #]

%% Clusters
\test ##[ { \makeClusters { c4 g4 } } #]		% ClusterNoteEvent

%% Figured bass
\test ##[ \figures { < 6 >4 } #]			% BassFigureEvent
\test ##[ \figuremode { < 1-- 3- >4 < 2+ 4++ >4 < _! 7! >4 } #]
\test ##[ \figuremode { < [6 >4 < 5] >4 } #]

%% Lyrics
\test ##[ \lyrics { a b } #]
\test ##[ \lyricmode { a -- b } #] 		% HyphenEvent
\test ##[ \lyricmode { a __ b } #] 		% ExtenderEvent
\test ##[ \lyricmode { "a " } #] 			% LyricEvent
\test ##[ \lyricsto "foo" { bla bla  } #]		% LyricCombineMusic
\test ##[ { { c4 d4 }
  \addlyrics { bla bla  } } #]
\test ##[ \lyricmode { a \vowelTransition b } #]	% VowelTransitionEvent

%% Drums
\test ##[ \drums { hihat4 } #]
\test ##[ \drummode { hihat4.*3/4 } #]

%% Expressive marks
\test ##[ c4 ~ #]			% TieEvent
\test ##[ c4\laissezVibrer #]		% LaissezVibrerEvent
\test ##[ c4^\laissezVibrer #]
\test ##[ c4\repeatTie #]		% RepeatTieEvent
\test ##[ c4^\repeatTie #]
\test ##[ c4\noBeam #] 			% BeamForbidEvent
\test ##[ c4\1 #] 			% StringNumberEvent
\test ##[ { c4:8 c4:1 } #]		% TremoloEvent
\test ##[ { c4-^ c4^^ c4_^ } #]		% ArticulationEvent
\test ##[ { c4-+ c4^+ c4_+ } #]
\test ##[ { c4-- c4^- c4_- } #]
\test ##[ { c4-! c4^! c4_! } #]
\test ##[ { c4-> c4^> c4_> } #]
\test ##[ { c4-. c4^. c4_. } #]
\test ##[ { c4-_ c4^_ c4__ } #]
\test ##[ { c4\trill c4^\trill c4_\trill } #]
\test ##[ { R1-! R1-- R1\trill R1^"a" R1^\markup \italic "foo" } #]
\test ##[ { c4-1 c4^2 c4_3 } #]				% FingerEvent
\test ##[ { c4-"foo" c4^"foo" c4_"foo" } #]		% TextScriptEvent
\test ##[ { R1*4-"foo" R1*4^"foo" R1*4_"foo" } #]	% MultiMeasureTextEvent
\test ##[ { < c\harmonic >4 < c e\harmonic >4 } #] 	% HarmonicEvent
\test ##[ { c4\glissando c4^\glissando c4_\glissando } #]	% GlissandoEvent
\test ##[ { c4\arpeggio c4^\arpeggio c4_\arpeggio } #] 	% ArpeggioEvent
\test ##[ { c4\p c4^\ff c4_\sfz } #] 			% AbsoluteDynamicEvent
\test ##[ { c4\< c,,4\!\breakDynamicSpan\p } #]          % BreakDynamicSpanEvent
\test ##[ { c4[ c4] c4^[ c4^] c4_[ c4_] } #] 		% BeamEvent
\test ##[ { c4( c4) c4^( c4^) c4_( c4_) } #] 		% SlurEvent
\test ##[ { c4\< c4\! c4^\< c4^\! c4_\< c4_\! } #]	% CrescendoEvent
\test ##[ { c4\> c4\! c4^\> c4^\! c4_\> c4_\! } #]	% DecrescendoEvent
\test ##[ { c4\episemInitium c4\episemFinis } #]		% EpisemaEvent
\test ##[ { c4\( c4\) c4^\( c4^\) c4_\( c4_\) } #]	% PhrasingSlurEvent
\test ##[ { c4\sustainOn c4\sustainOff } #]		% SustainEvent
\test ##[ { c4\sostenutoOn c4\sostenutoOff } #]		% SostenutoEvent
\test ##[ \melisma #]
\test ##[ \melismaEnd #]
\test ##[ { c4\startTextSpan c4\stopTextSpan } #]		% TextSpanEvent
\test ##[ { c4\startTrillSpan c4\stopTrillSpan } #]	% TrillSpanEvent
\test ##[ { c4 \startStaff c4 \stopStaff } #]		% StaffSpanEvent
\test ##[ { c4\startGroup c4\stopGroup c4^\startGroup c4^\stopGroup c4_\startGroup c4_\stopGroup } #]    % NoteGroupingEvent
\test ##[ { c4\unaCorda c4\treCorde } #]			% UnaCordaEvent
\test ##[ \breathe #]					% BreathingEvent
\test ##[ \caesura #]					% CaesuraEvent
\test ##[ { c4 \[ c4 \] } #]				% LigatureEvent
\test ##[ \~ #]						% PesOrFlexaEvent
\test ##[ \- #]						% DurationLineEvent
\test ##[ { c4\glide-3 c4-3 } #]	% FingerGlideEvent
\test ##[ \^ #]						% BendSpanEvent
\test ##[ c4\bendAfter #3 #]				% BendAfterEvent
\test ##[ c4\rightHandFinger #1 #]			% StrokeFingerEvent

\test ##[ \break #]		% LineBreakEvent
\test ##[ \noBreak #]
\test ##[ \pageBreak #]		% PageBreakEvent
\test ##[ \noPageBreak #]
\test ##[ \pageTurn #]		% PageTurnEvent
\test ##[ \noPageTurn #]
\test ##[ \label #'labI #]	%  LabelEvent
\test ##[ \newSpacingSection #]	% SpacingSectionEvent

%% Checks
\test ##[ \octaveCheck a' #]				% RelativeOctaveCheck
\test ##[ | #]						% BarCheck

%% Marks
\test ##[ \mark \default #]			% RehearsalMarkEvent
\test ##[ \mark #3 #]				% RehearsalMarkEvent
\test ##[ \mark "Z" #]				% AdHocMarkEvent
\test ##[ \mark \markup \italic "X" #]
\test ##[ \mark "Allegro" #]
\test ##[ \textMark "Fl. 1 solo" #]              % TextMarkEvent
\test ##[ \textEndMark \markup \italic "Seconda volta duo" #]
\test ##[ \tempo 4 = 120 #]			% MetronomeChangeEvent
\test ##[ \tempo 4 = 108 - 116 #]
\test ##[ \tempo "Allegro" 4 = 132 #]
\test ##[ \tempo "Andante" #]
\test ##[ \jump \markup \italic "Gavotte I D.C." #]	% AdHocJumpEvent
\test ##[ \sectionLabel \markup \italic "Gavotte" #]	% SectionLabelEvent

%% Segno
\test ##[ \segnoMark \default #]
\test ##[ \segnoMark #2 #]
\test ##[ c4-\markup \segno #]

%% Coda sign
\test ##[ \codaMark \default #]
\test ##[ \codaMark #2 #]
\test ##[ c4-\markup \coda #]
\test ##[ c4-\markup \varcoda #]

%% key, time, clef, bar
\test ##[ \key \default #]			% KeyChangeEvent
\test ##[ \key e \minor #]
\test ##[ \clef "bass" #]
\test ##[ \clef "french^2" #]
\test ##[ \clef "treble_[8]" #]
\test ##[ \clef "bass^(15)" #]
\test ##[ \clef "alto_3" #]
\test ##[ \time 2/4 #]
\test ##[ \time 3,2 5/8 #]
\test ##[ \bar "|." #]
\test ##[ \fine #]
\test ##[ \section #]

%% staff switches
\test ##[ \autoChange { c4 d4 } #]			% AutoChangeMusic
\test ##[ { \change Staff = "up" { c4 d4 } } #]		% ContextChange

%% Tuplets
\test ##[ \tuplet 3/2 { c8 d8 e8 } #]				% TimeScaledMusic
\test ##[ \tuplet 6/4 { c16 d16 e16 f16 g16 a16 } #]
\test ##[ \tuplet 3/2 { c4 d4 e4 \tuplet 5/2 { f4 e4 d2 d4 } c4 } #]
\test ##[ \tuplet 3/2 2 { c4 d4 e4 \tuplet 5/2 2 { f4 e4 d2 d4 } c4 } #]

%% pure rhythm
\test ##[ { 4 4 8 \tuplet 3/2 { 8[ 16] } 16 } #]

%% \relative and \transpose
\test "\absolute { c'4 f'4 }"
      ##[ \relative { c'4 f4 } #]			% RelativeOctaveMusic
\test "{ d4 e4 }"
      ##[ \transpose c d { c4 d4 } #]			% TransposedMusic

%% Repeats
\test ##[ \repeat segno 2 { c4 d4 } #]			% SegnoRepeatedMusic
\test ##[ \repeat volta 2 { c4 d4 } #]			% VoltaRepeatedMusic
\test ##[ \repeat unfold 2 { c4 d4 } #]			% UnfoldedRepeatedMusic
\test ##[ \repeat percent 2 { c4 d4 } #]			% PercentRepeatedMusic
\test ##[ \repeat tremolo 4 { c16 d16 } #]		% TremoloRepeatedMusic
\test ##[ \repeat tremolo 7 { c''32 b'32 } #]
\test ##[ \repeat tremolo 15 { c''16 b'16 } #]

\test "\repeat segno 2 c4 \alternative { \volta 1 d4 \volta 2 e4 }"
      ##[ \repeat segno 2 c4 \alternative { d4 e4 } #]
\test "\repeat volta 2 c4 \alternative { \volta 1 d4 \volta 2 e4 }"
      ##[ \repeat volta 2 c4 \alternative { d4 e4 } #]
\test "\repeat unfold 2 c4 \alternative { \volta 1 d4 \volta 2 e4 }"
      ##[ \repeat unfold 2 c4 \alternative { d4 e4 } #]

\test "\repeat segno 2 { a1 \alternative { \volta 1 b1 \volta 2 c1 } d1 }"
      ##[ \repeat segno 2 { a1 \alternative { b1 c1 } d1 } #]
\test "\repeat volta 2 { a1 \alternative { \volta 1 b1 \volta 2 c1 } d1 }"
      ##[ \repeat volta 2 { a1 \alternative { b1 c1 } d1 } #]
\test "\repeat unfold 2 { a1 \alternative { \volta 1 b1 \volta 2 c1 } d1 }"
      ##[ \repeat unfold 2 { a1 \alternative { b1 c1 } d1 } #]

\test ##[ \alternative { b1 c1 } #] % not really expected outside of \repeat
\test ##[ \unfolded R1 #]
\test ##[ \volta 1 c'1 #]
\test ##[ \volta 5,13,12 c'1 #]

%% Context creation
\test ##[ \new Staff { c4 d4 } #]		% ContextSpeccedMusic
\test ##[ \new Staff = "up" { c4 d4 } #]		% ContextSpeccedMusic
\test ##[ \context Staff { c4 d4 } #]
\test ##[ \context Staff = "up" { c4 d4 } #]
\test ##[
\new Staff \with {
  \consists "Timing_engraver"
  \remove "Clef_engraver"
} { c4 d4 } #]
%% Context properties
\test ##[ \once \set Score.skipBars = ##t #]		% PropertySet
\test ##[ \set autoBeaming = ##f #]
\test ##[ \unset Score.skipBars #]			% PropertyUnset
\test ##[ \unset autoBeaming #]
%% Layout properties
\test ##[ \override Staff.Stem.thickness = #4.0 #]	% OverrideProperty
\test ##[ \once \override Beam.beam-thickness = #0.6 #]
\test ##[ \revert Staff.Stem.thickness #]		% RevertProperty
\test ##[ \revert Beam.beam-thickness #]
\test "NOT A BUG" ##[ \oneVoice #]	% resetting a bunch of properties
\test ##[ \override StaffGrouper.staff-staff-spacing.basic-distance = #7 #] % nested properties
\test ##[ \revert StaffGrouper.staff-staff-spacing.basic-distance #] % nested properties

%% \applyOutput
\test ##[ \applyOutput Foo #identity #]
\test ##[ \applyOutput Foo.NoteHead #identity #]
%% \applyContext
\test ##[ \applyContext #identity #]

%% \partial
\test ##[ \partial 2 #]
\test ##[ \partial 8. #]
\test ##[ \partial 4*2/3 #]

%% \after
\test ##[ \after 4 \f d'2 #]
\test ##[ \after 4 \turn d'2 #]
\test ##[ \after 4 e'4 d'2 #]

%% \partCombine
\test ##[ \partCombine { c4 e4 }
{ d4 f4 } #]
\test ##[ \partCombineUp { c2 e2 }
{ d2 f2 } #]
\test ##[ \partCombineDown { c1 e1 }
{ d1 f1 } #]

%% Cue notes
\test ##[ \cueDuring "foo" #1 { c4 d4 } #]
\test ##[ \quoteDuring "foo" { c4 d4 } #]

%% \ottava
\test ##[ \ottava #1 #]	% OttavaEvent

%% \tweak
\test ##[ < \tweak duration-log #2 c >4 #]
\test ##[ < c \tweak transparent ##t e >4 #]
\test ##[ < \tweak color #'(1.0 0.0 0.0) \tweak duration-log #2 c >4 #]
\test ##[ c4-\tweak font-size #3 -> #]
\test ##[ < \tweak Accidental.color #'(1.0 0.0 0.0) cis eis g >4 #]

%% highlights
\test ##[ \staffHighlight "red" #]	% StaffHighlightEvent
\test ##[ \stopStaffHighlight #]

%% measure counters and spanners
\test ##[ \startMeasureCount #]		% MeasureCounterEvent
\test ##[ \stopMeasureCount #]
\test ##[ \startMeasureSpanner #]	% MeasureSpannerEvent
\test ##[ \stopMeasureSpanner #]

%% footnotes
%\test ##[ \footnote #'(1 . 2) "footnote" NoteHead #] % FootnoteEvent
%\test ##[ \footnote "*" #'(1 . 2) "footnote" c' #]


%% balloons
\test ##[ \balloonGrobText #'NoteHead #'(1 . 2) "note head" #]	% AnnotateOutputEvent

%% grouped post-events
\test ##[ ^"a"^\fermata #] % PostEvents

%% these types do not implement a display method on purpose
\ignoreMusicType TremoloSpanEvent
\ignoreMusicType RepeatSlashEvent
\ignoreMusicType PercentEvent
\ignoreMusicType DoublePercentEvent
\ignoreMusicType VoltaSpanEvent
\ignoreMusicType VoltaRepeatStartEvent
\ignoreMusicType VoltaRepeatEndEvent
\ignoreMusicType AlternativeEvent
\ignoreMusicType DalSegnoEvent
\ignoreMusicType SpanEvent
\ignoreMusicType Event
\ignoreMusicType MultiMeasureRestEvent
\ignoreMusicType ScriptEvent % unused?
\ignoreMusicType UnisonoEvent
\ignoreMusicType SoloOneEvent
\ignoreMusicType SoloTwoEvent
\ignoreMusicType TupletSpanEvent
\ignoreMusicType TimeSignatureEvent % unused?

%% TODO: this one should be implemented, but currently
%% isn't.  The implementation is complicated by the way
%% \footnote works, adding a music as tweak.  It should
%% probably be simplified anyway.
\ignoreMusicType FootnoteEvent

%% end test.

#(read-hash-extend #\[ #f) %{ ] %}

#(for-each
  (match-lambda
   ((name . props)
    (when (not (hashq-ref tested-music-types name))
      (ly:error "no display-lily test for music type ~a"
                name))))
  music-descriptions)
