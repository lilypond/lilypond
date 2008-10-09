%******************************************************
% Inspirational header for Unfretted Strings section  %
% of Lilypond Documentation.  This passage is taken   %
% from Ravel's String Quartet.			      %
% ****************************************************%

%\version "2.11.61"

#(set-global-staff-size 15)
\paper{
 ragged-end=##t
 line-width=17\cm
 indent=0\cm
}

\layout {
 \context { \Score
   \remove "Bar_number_engraver"
   \override PaperColumn #'keep-inside-line = ##t
   \override NonMusicalPaperColumn #'keep-inside-line = ##t
 }
}

%*************** MACROS ******************%

#(define (octave-up m t)
 (let* ((octave (1- t))
      (new-note (ly:music-deep-copy m))
      (new-pitch (ly:make-pitch
        octave
        (ly:pitch-notename (ly:music-property m 'pitch))
        (ly:pitch-alteration (ly:music-property m 'pitch)))))
  (set! (ly:music-property new-note 'pitch) new-pitch)
  new-note))

#(define (octavize-chord elements t)
 (cond ((null? elements) elements)
     ((eq? (ly:music-property (car elements) 'name) 'NoteEvent)
       (cons (car elements)
             (cons (octave-up (car elements) t)
                   (octavize-chord (cdr elements) t))))
     (else (cons (car elements) (octavize-chord (cdr elements ) t)))))

#(define (octavize music t)
 (if (eq? (ly:music-property music 'name) 'EventChord)
       (ly:music-set-property! music 'elements (octavize-chord
(ly:music-property music 'elements) t)))
 music)

octaves = #(define-music-function (parser location arg mus) (integer? ly:music?)
 (music-map (lambda (x) (octavize x arg)) mus))

%\relative c' { d e \octaves #-1 { \times 2/3 {f g c }}} % this is an example of the macro in practice

%*********************************************************************
% This is a sweet macro by Mark Polesky to make the 4th-string
% indication look like it did in the original score
%********************************************************************
#(define-markup-command (No layout props n) (string?)
  (define (format-char c)
    (let ((s (string c)))
      (if (number? (string->number s))
          (markup #:hspace 0.125 #:number s #:hspace 0.125)
          (markup #:hspace 0 #:fontsize 2 s))))
  (define (format-string s i)
    (let ((n (string-length s)))
      (if (= n 0)
          (markup #:null)
          (markup (format-char (string-ref s i))
                  (if (= (+ i 1) n)
                      (markup #:null)
                      (format-string s (+ i 1)))))))
  (let ((i (string-length n)))
    (interpret-markup layout props
      (markup
        #:concat (
          #:concat (
            #:fontsize 3 "4"
            #:hspace 0.5
            #:override '(baseline-skip . 0.825)
            #:override '(direction . 1)
            #:dir-column (
              #:fontsize 2 #:center-align "."
              #:fontsize 1 #:center-align "e"
            )
          )
          #:hspace 0.75
          #:concat (
            (format-string n 0)
            #:fontsize 2 ""
          )
        )
      )
    )
  )
)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5


poco = \markup { \italic { \fontsize #+2.0 "poco" }}
apoco = \markup { \italic { \fontsize #+2.0 "a" }}
menovivo = \markup { \bold { \fontsize #+3.0 { "Poco meno vivo"} } }
pocorit = \markup { \bold { \fontsize #+3.0 { "poco rit."} } }
pocodim = \markup { \italic { \fontsize #+2.0 "un poco dim." }}

vlnOne = \relative c''' {
  \key f \major
  \time 4/4
  \set Staff.midiInstrument = "violin"
%  \override TupletNumber #'stencil = ##f
  \override TupletBracket #'bracket-visibility = ##f
  \set tupletSpannerDuration = #(ly:make-moment 1 4)

  \once \override TextScript #'extra-offset = #'( -3.8 . 0.5 )
  es4^\apoco
  \once \override TextScript #'extra-offset = #'( -1.0 . -0.8 )
  \times 2/3 { c8(^\poco d es) } es4 \times 2/3 { c8( d es) } |	% 1
  es4\ff\< \times 2/3 { c8( d es) } es4 \times 2/3 { c8( d es)\! }  |	% 2
  \grace { a,,8[( a'\fff]^\menovivo } a'4) g8( a) d,4 c8( d) | 	% 3
  a4 g8( a) d,4 c8( d) |		% 4
  a4 g8( a) d,8^\pocorit r8 r4 | 	% 5
  ais16(\p\< e' ais e') e16(\f\> ais, e ais,)\! ais8 r r4 |	% 6
}

vlnTwo = \relative c' {
  \key f \major
  \time 4/4
  \set Staff.midiInstrument = "violin"
%  \override TupletNumber #'stencil = ##f
  \override TupletBracket #'bracket-visibility = ##f
  \set tupletSpannerDuration = #(ly:make-moment 1 4)
  \times 2/3 { \octaves #1 {fis8 fis fis fis fis fis fis fis fis fis fis fis }} |	% 1
  \times 2/3 { \octaves #1 {fis8\ff\< fis fis fis fis fis fis fis fis fis fis fis\! }} |% 2
  <ais, e' ais e'>16\fff( ais' e ais,) ais16( e' ais e') e16( ais, e ais,) ais16( e' ais e') | % 3
  e16( ais, e ais,) ais16( e' ais e') e16( ais, e ais,) ais16( e' ais e') |	% 4
  e16( ais, e ais,)
  \set subdivideBeams = ##t
  \set Score.beatLength = #(ly:make-moment 1 8)
    ais16( e') e( ais,)
    d4^\markup \No #"Corde"
  \once \override TextScript #'extra-offset = #'( -5.5 . 4.4 )
    c8(_\markup { \italic { \fontsize #+1.0 "vibrato" }} d) |	% 5
  \set subdivideBeams = ##f
  a2 ~ a8 g( c d)			% 6
}
vlnTwoDyn = {
  s1*4
  s2 s4\f\< s8. s16\! |
  s16 s16\> s8 s2 s8 s16 s32 s32\!

}

vla = \relative c'' {
  \key f \major
  \time 4/4
  \clef "alto"
  \set Staff.midiInstrument = "viola"
  \override Stem #'length = #9.5
  bes4:32 a4:32 gis4:32 a4:32 | % 1
  bes4:32 a4:32 gis4:32 a4:32\! | % 2
\tweak #'extra-offset #'(-1.3 . -0.0)
%  \clef "treble"
  \revert Stem #'length
  \acciaccatura { a8 } a'4\fff g8( a) d,4 c8( d)  \clef "alto"	| %3
  a4 g8( a) d,4 c8( d) |	% 4
  a4 g8 a <e ais>2:32 |		% 5
  <e ais>1:32		|	% 6
}

vlaDyn = {
  s1 | s2\ff\< s4 s8 s16\! s32 s64 \clef "treble" s64 |
  s1*2 | s2 s4\f\< s8 s16 s32 s32\! |	% 3-5
  s16 s16\> s8 s2 s8 s16 s32 s32\! | %6

}

vc = \relative c' {
  \key f \major
  \time 4/4
  \clef "bass"
  \set Staff.midiInstrument = "cello"
  \override Stem #'length = #9.5
  c4:32 d4:32 es4:32 d4:32 |		% 1
  c4:32 d4:32 es4:32 d4:32\! |	% 2
  \revert Stem #'length
  \set subdivideBeams = ##t
  \set Score.beatLength = #(ly:make-moment 1 8)
  <fis,, cis'>16\fff( <cis' ais'>) <cis ais'>( <fis, cis'>) % 3
    <fis cis'>16( <cis' ais'>) <cis ais'>( <fis, cis'>)	% 3
    <fis c'>16( <c' ais'>) <c ais'>( <fis, c'>) 	% 3
    <fis c'>16( <c' ais'>) <c ais'>( <fis, c'>) | 	% 3
  <fis cis'>16( <cis' ais'>) <cis ais'>( <fis, cis'>)	% 4
    <fis cis'>16( <cis' ais'>) <cis ais'>( <fis, cis'>)	% 4
    <fis c'>16( <c' ais'>) <c ais'>( <fis, c'>) 	% 4
    <fis c'>16( <c' ais'>) <c ais'>( <fis, c'>) | 	% 4
  <fis cis'>16( <cis' ais'>) <cis ais'>( <fis, cis'>)	% 5
    <fis cis'>16( <cis' ais'>) <cis ais'>( <fis, cis'>)	fis2^\( ~ | % 5
  fis4 g c,\) r4	% 6
}
vcDyn = {
  s1 | s2\ff\< s4 s8. s16\! |	% 1-2
  s1*2	% 3-4
  s2 s4\f\< s8 s16 s32 s32\! |	% 5
  s4\> s4 s4\! s4	     |  % 6
}


%%%%%%%%%%%%%%%% Score Block %%%%%%%%%%%%%%%%%%%%%%

\score {

  << % creates new grand staff
    \new StaffGroup = "strings" <<
      \context Staff = "violinOne" \vlnOne
      \context Staff = "violinTwo" <<
        \context Voice = "violin 2" { \vlnTwo }
        \context Voice = "violin 2 dynamics" { \vlnTwoDyn }
      >>
      \new Staff = "viola" <<
        \context Voice = "viola" { \vla }
        \context Voice = "viola dynamics" { \vlaDyn }
      >>
      \new Staff = "cello" <<
        \context Voice = "cello" { \vc }
        \context Voice = "cello dynamics" { \vcDyn }
      >>
    >> % end of "strings" staffgroup

  >> % end of grand staff

  \layout {
    \context {
      \Score
      \override TimeSignature #'stencil = ##f
      \override BarNumber #'padding = #3
      \override RehearsalMark #'padding = #2
        skipBars = ##t
    } % context \Score \overrides end
  } % layout end

  \midi { }

} % score end

