\version "2.27.1"

\header {
  texidoc = "This is a set of tests of the @code{find-leading-partial} Scheme function.
Problems are reported in the standard error stream."
}

%% Direct \partial
mDirect = { \partial 4 c'4 }

%% \partial preceded by \clef
mAfterClef = { \clef alto \partial 8 c'8 }

%% \partial preceded by \time and \key
mAfterTimeKey = { \time 3/4 \key g \major \partial 4. c'4. }

%% \partial inside \new Staff (ContextSpeccedMusic)
mInStaff = \new Staff { \partial 8 b'8 }

%% \partial in sequentual music
mSequential = <<
  { c'4 }
  { \partial 4 d'4 }
>>

%% No \partial at all
mNoPartial = { c'4 d' e' f' }

%% music before \partial (should not be detected as partial)
mMusicBeforePartial = { c'4 \partial 4 d'4 }

#(define (partial? m)
   (and (ly:music? m)
        (eq? (ly:music-property m 'name) 'PartialSet)))

#(assert (partial? (find-leading-partial mDirect)))
#(assert (partial? (find-leading-partial mAfterClef)))
#(assert (partial? (find-leading-partial mAfterTimeKey)))
#(assert (partial? (find-leading-partial mInStaff)))
#(assert (partial? (find-leading-partial mSequential)))
#(assert (not (find-leading-partial mNoPartial)))
#(assert (not (find-leading-partial mMusicBeforePartial)))
