\version "2.23.4"

\header {
  texidoc = "This tests the calculation of music start and length for
various kinds of music.  Problems are reported on stderr."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "skipping zero-duration score"))
#(ly:expect-warning (G_ "to suppress this, consider adding a spacer rest"))

\fixed c' <<

\testStartAndLength \partial 1*0
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength { \partial 1*0 }
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength << \partial 1*0 >>
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \partial 8
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength { \partial 8 }
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength << \partial 8 >>
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength r8
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength { r8 }
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength << r8 >>
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength s8
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength { s8 }
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength << s8 >>
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength \skip 8
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength { \skip 8 }
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength << \skip 8 >>
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength \unfoldRepeats \skip \repeat volta 2 c8
#ZERO-MOMENT
#(ly:make-moment 1/4 0)

\testStartAndLength <d>8
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength { <d>8 }
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength { <d>8 q }
#ZERO-MOMENT
#(ly:make-moment 1/4 0)

\testStartAndLength c8
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength { c8 }
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength << c8 >>
#ZERO-MOMENT
#(ly:make-moment 1/8 0)

\testStartAndLength { c8 c4 }
#ZERO-MOMENT
#(ly:make-moment 3/8)

\testStartAndLength << c8 c4 >>
#ZERO-MOMENT
#(ly:make-moment 1/4)

\testStartAndLength \times 2/3 c4.
#ZERO-MOMENT
#(ly:make-moment 1/4)

\testStartAndLength \tuplet 3/2 4 c4.
#ZERO-MOMENT
#(ly:make-moment 1/4)

>>
