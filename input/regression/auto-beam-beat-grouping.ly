\version "2.12.0"
\header {

  texidoc = "Autobeaming will break beams according to beatGrouping
if the total length of the beat groups is equal to measureLength.
Otherwise, it will break beams according to beatLength."
}

{
  \time 12/16

  % default beatLength is 1/16; beatGrouping for this time is '()
  b16 b b b b b b b b b b b % beatlength is used to breatk these beams
  % use beatGrouping to get 1/8 groups
  \set Timing.beatGrouping = #'(2 2 2 2 2 2) % 6*2 = 12 so beatGrouping applies
  b16 b b b b b b b b b b b  %  beam groups are 1/8
  % use beatLength to get 1/8 groups -- beatGrouping no longer applies
  \set Score.beatLength = #(ly:make-moment 2 16 )  % 12*2/16 = 24/16
                                                   % bad beatGrouping; use
                                                   % beatLength (1/8 notes)
  b16 b b b b b b b b b b b
  % make custom beatGrouping
  \set Timing.beatGrouping = #'(3 1 2)  % 6*2/16 = 12/16
  b16 b b b b b b b b b b b
  % change beatLength
  \set Score.beatLength = #(ly:make-moment 3 16 )  % 6*3/16 = 18/16; use beatLength
  b16 b b b b b b b b b b b
  \set Score.beatLength = #(ly:make-moment 4 16 )  % 6*4/16 = 24/16; use beatLength
  b16 b b b b b b b b b b b
}
