\version "2.27.2"

\header {
  texidoc = "This tests the equivalence of @samp{\\measureRemainder
@var{music}} and @samp{\\setMeasureLengthFromHere @var{duration} @var{music}
... | \\setDefaultMeasureLength}.  The two scores should look identical.

The beaming pattern is @code{1,2,3,4}.  Measure@tie{}1 has only two eighth
notes.  Measure@tie{}2 has six.  Measure@tie{}3 has ten.  Measure@tie{}4 is a
normal measure."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Score
    barNumberVisibility = #(every-nth-bar-number-visible 1)
    \override BarNumber.break-visibility = #all-visible
     \overrideTimeSignatureSettings 4/4 1/8 1,2,3,4 #'()
  }
}

\score {
  \new Staff \with {
    instrumentName = "sets"
  } \fixed c' {
    \setMeasureLengthFromHere 4 c8 c |           % start of score
    \setDefaultMeasureLength

    d8 d \setMeasureLengthFromHere 2 e8 e e e |  % mid measure
    \setDefaultMeasureLength

    \setMeasureLengthFromHere 4*5 \*10 f8 |      % start of measure
    \setDefaultMeasureLength
    g2.. g8
  }
}

\score {
  \new Staff \with {
    instrumentName = "music"
  } \fixed c' {
    \measureRemainder { c8 c }                    % start of score
    d8 d \measureRemainder { e8 e e e }           % mid measure
    \measureRemainder { \*10 f8 }                 % start of measure
    g2.. g8
  }
}
