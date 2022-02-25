\version "2.23.0"

\header {
  texidoc = "This tests the calculation of music start and length for
various kinds of repeated music with alternatives.  Problems are
reported on stderr."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "skipping zero-duration score"))
#(ly:expect-warning (G_ "to suppress this, consider adding a spacer rest"))

\fixed c' <<

%% zero-duration body
%% zero-duration alternatives

\testStartAndLength \repeat unfold 2 s1*0 \alternative { s1*0 }
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \repeat volta 2 s1*0 \alternative { s1*0 }
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats {
  \repeat unfold 2 s1*0 \alternative { s1*0 }
}
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats \removeWithTag "X" {
  \repeat unfold 2 s1*0 \alternative { s1*0 }
}
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \removeWithTag "X" \unfoldRepeats {
  \repeat unfold 2 s1*0 \alternative { s1*0 }
}
#ZERO-MOMENT
#ZERO-MOMENT

%% zero-duration body
%% alternatives have grace duration only

\testStartAndLength \repeat unfold 4 s1*0 \alternative {
  { \grace c8 \grace d8 }
  \grace e8
  s1*0
  { \grace f8 \grace g8 }
}
#(ly:make-moment 0 -5/8)
#ZERO-MOMENT

\testStartAndLength \repeat volta 4 s1*0 \alternative {
  { \grace c8 \grace d8 }
  \grace e8
  s1*0
  { \grace f8 \grace g8 }
}
#(ly:make-moment 0 -5/8)
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats {
  \repeat volta 4 s1*0 \alternative {
    { \grace c8 \grace d8 }
    \grace e8
    s1*0
    { \grace f8 \grace g8 }
  }
}
#(ly:make-moment 0 -5/8)
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats \removeWithTag "X" {
  \repeat volta 4 s1*0 \alternative {
    { \grace c8 \grace d8 }
    \grace e8
    s1*0
    { \grace f8 \grace g8 }
  }
}
#(ly:make-moment 0 -5/8)
#ZERO-MOMENT

\testStartAndLength \removeWithTag "X" \unfoldRepeats {
  \repeat volta 4 s1*0 \alternative {
    { \grace c8 \grace d8 }
    \grace e8
    s1*0
    { \grace f8 \grace g8 }
  }
}
#(ly:make-moment 0 -5/8)
#ZERO-MOMENT

%% body and alternatives are grace notes only

\testStartAndLength \repeat unfold 2 \grace c8 \alternative {
  \grace d8
  \grace e8
}
#(ly:make-moment 0 -4/8)
#ZERO-MOMENT

\testStartAndLength \repeat volta 2 \grace c8 \alternative {
  \grace d8
  \grace e8
}
#(ly:make-moment 0 -3/8)
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats {
  \repeat volta 2 \grace c8 \alternative {
    \grace d8
    \grace e8
  }
}
#(ly:make-moment 0 -4/8)
#ZERO-MOMENT

\testStartAndLength \unfoldRepeats \removeWithTag "X" {
  \repeat volta 2 \grace c8 \alternative {
    \grace d8
    \grace e8
  }
}
#(ly:make-moment 0 -4/8)
#ZERO-MOMENT

\testStartAndLength \removeWithTag "X" \unfoldRepeats {
  \repeat volta 2 \grace c8 \alternative {
    \grace d8
    \grace e8
  }
}
#(ly:make-moment 0 -4/8)
#ZERO-MOMENT

%% body is a grace note
%% alternative is a main note

\testStartAndLength \repeat unfold 3 \grace c8 \alternative { d4 }
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

\testStartAndLength \repeat volta 3 \grace c8 \alternative { d4 }
#(ly:make-moment 0 -1/8)
#(ly:make-moment 1/4)

\testStartAndLength \unfoldRepeats {
  \repeat volta 3 \grace c8 \alternative { d4 }
}
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

\testStartAndLength \unfoldRepeats \removeWithTag "X" {
  \repeat volta 3 \grace c8 \alternative { d4 }
}
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

\testStartAndLength \removeWithTag "X" \unfoldRepeats {
  \repeat volta 3 \grace c8 \alternative { d4 }
}
#(ly:make-moment 0 -1/8)
#(ly:make-moment 3/4)

%% body and alternatives are main notes only

\testStartAndLength \repeat unfold 2 c4 \alternative { d4 }
#ZERO-MOMENT
#(ly:make-moment 4/4)

\testStartAndLength \repeat volta 2 c4 \alternative { d4 }
#ZERO-MOMENT
#(ly:make-moment 2/4)

\testStartAndLength \unfoldRepeats {
  \repeat volta 2 c4 \alternative { d4 }
}
#ZERO-MOMENT
#(ly:make-moment 4/4)

\testStartAndLength \unfoldRepeats \removeWithTag "X" {
  \repeat volta 2 c4 \alternative { d4 }
}
#ZERO-MOMENT
#(ly:make-moment 4/4)

\testStartAndLength \removeWithTag "X" \unfoldRepeats {
  \repeat volta 2 c4 \alternative { d4 }
}
#ZERO-MOMENT
#(ly:make-moment 4/4)

>>
