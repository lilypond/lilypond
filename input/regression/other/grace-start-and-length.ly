\version "2.23.4"

\header {
  texidoc = "This tests the calculation of music start and length for
various kinds of grace music.  Problems are reported on stderr."
}

\include "testing-functions.ily"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "skipping zero-duration score"))
#(ly:expect-warning (G_ "to suppress this, consider adding a spacer rest"))

\fixed c' <<

\testStartAndLength \grace \partial 1*0
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \grace { \partial 1*0 }
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \grace << \partial 1*0 >>
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \grace \partial 8
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \grace { \partial 8 }
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \grace << \partial 8 >>
#ZERO-MOMENT
#ZERO-MOMENT

\testStartAndLength \grace r8
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace { r8 }
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace << r8 >>
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace s8
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace { s8 }
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace << s8 >>
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace \skip 8
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace { \skip 8 }
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace << \skip 8 >>
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace <d>8
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace { <d>8 }
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace { <d>8 q }
#(ly:make-moment 0 -1/4)
#ZERO-MOMENT

\testStartAndLength { \grace <d>8 q }
#(ly:make-moment 0 -1/8)
#(ly:make-moment 1/8)

\testStartAndLength \grace c8
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace { c8 }
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength \grace << c8 >>
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength { \grace c8 }
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength << \grace c8 >>
#(ly:make-moment 0 -1/8)
#ZERO-MOMENT

\testStartAndLength { \grace c8 c4 }
#(ly:make-moment 0 -1/8)
#(ly:make-moment 1/4)

\testStartAndLength << \grace c8 c4 >>
#(ly:make-moment 0 -1/8)
#(ly:make-moment 1/4)

\testStartAndLength \grace { c8 c4 }
#(ly:make-moment 0 -3/8)
#ZERO-MOMENT

\testStartAndLength \grace << c8 c4 >>
#(ly:make-moment 0 -1/4)
#ZERO-MOMENT

\testStartAndLength \grace \times 2/3 c4.
#(ly:make-moment 0 -1/4)
#ZERO-MOMENT

\testStartAndLength \times 2/3 \grace c4.
#(ly:make-moment 0 -1/4)
#ZERO-MOMENT

\testStartAndLength \tuplet 3/2 4 \grace c4.
#(ly:make-moment 0 -1/4)
#ZERO-MOMENT

\testStartAndLength \grace \tuplet 3/2 4 c4.
#(ly:make-moment 0 -1/4)
#ZERO-MOMENT

>>
