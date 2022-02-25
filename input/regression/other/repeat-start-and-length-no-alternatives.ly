\version "2.23.0"

\header {
  texidoc = "This tests the calculation of music start and length for
various kinds of repeated music without alternatives.  Problems are
reported on stderr."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "skipping zero-duration score"))
#(ly:expect-warning (G_ "to suppress this, consider adding a spacer rest"))

\fixed c' <<

%% zero-duration body

\testStartAndLength \repeat percent 3 s1*0
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \repeat tremolo 3 s1*0
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \repeat tremolo 3 { s1*0 s }
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \repeat unfold 3 s1*0
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \repeat volta 3 s1*0
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats {
  \repeat volta 3 s1*0
}
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats \removeWithTag "X" {
  \repeat volta 3 s1*0
}
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \removeWithTag "X" \unfoldRepeats {
  \repeat volta 3 s1*0
}
#ZERO-MOMENT
#ZERO-MOMENT

%% body is two grace notes in separate elements

\testStartAndLength \repeat percent 3 { \grace c8 \grace d8 }
%% The percent implementation doesn't expect pure grace notes, but ...
#(ly:make-moment 0 -6/8) % this makes sense
#ZERO-MOMENT % this makes sense

\testStartAndLength \repeat tremolo 3 { \grace c8 \grace d8 }
%% The tremolo implementation doesn't expect grace notes, but ...
#(ly:make-moment 0 -6/8) % this makes sense
#ZERO-MOMENT % this makes sense

\testStartAndLength \repeat unfold 3 { \grace c8 \grace d8 }
#(ly:make-moment 0 -6/8)
#ZERO-MOMENT

\testStartAndLength \repeat volta 3 { \grace c8 \grace d8 }
#(ly:make-moment 0 -2/8)
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats {
  \repeat volta 3 { \grace c8 \grace d8 }
}
#(ly:make-moment 0 -6/8)
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats \removeWithTag "X" {
  \repeat volta 3 { \grace c8 \grace d8 }
}
#(ly:make-moment 0 -6/8)
#ZERO-MOMENT

\testStartAndLength \removeWithTag "X" \unfoldRepeats {
  \repeat volta 3 { \grace c8 \grace d8 }
}
#(ly:make-moment 0 -6/8)
#ZERO-MOMENT

%% body is a grace note and a main note

\testStartAndLength \repeat percent 3 { \grace c8 d4 }
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

\testStartAndLength \repeat tremolo 3 { \grace c8 d4 }
%% The tremolo implementation doesn't expect grace notes, but ...
#(ly:make-moment 0 -3/8) % TODO: -1/8 would probably make more sense
#(ly:make-moment 3/4) % this makes sense

\testStartAndLength \repeat unfold 3 { \grace c8 d4 }
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

\testStartAndLength \repeat volta 3 { \grace c8 d4 }
#(ly:make-moment 0 -1/8)
#(ly:make-moment 1/4)

\testStartAndLength \unfoldRepeats {
  \repeat volta 3 { \grace c8 d4 }
}
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

\testStartAndLength \unfoldRepeats \removeWithTag "X" {
  \repeat volta 3 { \grace c8 d4 }
}
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

\testStartAndLength \removeWithTag "X" \unfoldRepeats {
  \repeat volta 3 { \grace c8 d4 }
}
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

%% body has main time only

\testStartAndLength \repeat percent 3 d4
#ZERO-MOMENT
#(ly:make-moment 3/4)

\testStartAndLength \repeat tremolo 3 d4
#ZERO-MOMENT
#(ly:make-moment 3/4)

\testStartAndLength \repeat tremolo 3 { d8 e8 }
#ZERO-MOMENT
#(ly:make-moment 3/4)

\testStartAndLength \repeat unfold 3 d4
#ZERO-MOMENT
#(ly:make-moment 3/4)

\testStartAndLength \repeat volta 3 d4
#ZERO-MOMENT
#(ly:make-moment 1/4)

\testStartAndLength \unfoldRepeats \repeat volta 3 d4
#ZERO-MOMENT
#(ly:make-moment 3/4)

\testStartAndLength \unfoldRepeats \removeWithTag "X" \repeat volta 3 d4
#ZERO-MOMENT
#(ly:make-moment 3/4)

\testStartAndLength \removeWithTag "X" \unfoldRepeats \repeat volta 3 d4
#ZERO-MOMENT
#(ly:make-moment 3/4)

>>
