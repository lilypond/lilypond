\version "2.21.0"

\header {


  texidoc = " Figured bass is created by the FiguredBass context which
responds to figured bass events and rest events.  You must enter these
using the special @code{\\figuremode @{ @}} mode, which allows you to
type numbers, like @code{<4 6+>} and add slashes, backslashes and pluses.

You can also enter markup strings. The vertical alignment may also be tuned.

"

}


\paper { ragged-right = ##t }


<<
  \context Voice {
    \clef bass
    c 4
    c c c c c 
    g8
  }
  \figures { 
    <3 [5 7]>
    <3\+ [5/] 7/ [9 11]>
    <3+ 5- 7!>
    <3 _! 5 _- 7>
    <3 _\+ 5 _ 7>
    <3 6/ >
    <3 6\\ >
    <"V7" ["bla" 6] \markup{ \musicglyph "rests.2"} >

    \once \override BassFigureAlignment.stacking-dir = #UP
    <3 [5 7]>
    
  }
>>


