\version "2.21.0"

\header {
  texidoc = "Figured bass is created by the @code{FiguredBass} context,
which responds to figured bass and rest events.

You can also enter markup strings; the vertical alignment may also be
tuned."
}

<<
  \figures {
    \override BassFigureAlignment.stacking-dir = #UP
    s1
    <3 [5 7]>1
  }
  \context Voice {
    \clef bass
    c4 c c8 c c16 c c c | c1
  }
  \figures {
    <3 [5 7]>8
      <3\+ [5/] 7/ [9 11]>
    <3+ 5- 7!>
      <3 _! 5 _- 7>
    <3 _\+ 5 _ 7>
      <3 6/ >
    <3 6\\ >
      <"V7" ["bla" 6] \markup{ \musicglyph "rests.2"} >
  }
>>
