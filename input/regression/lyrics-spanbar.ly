\version "2.23.12"

\header {
  texidoc = "Empty measures and extraordinary bar-line thickness do
not confuse @code{SpanBarStub}.  These lyrics should remain clear of
the span bars.
"
}

\new StaffGroup <<
  \new Staff {
    R1 e'1
    R1 \override Score.BarLine.hair-thickness = 3 e'1
    R1 \override Score.BarLine.hair-thickness = 4 e'1
    R1 \override Score.BarLine.hair-thickness = 5 e'1
    R1 \override Score.BarLine.hair-thickness = 10 e'1
    R1 \override Score.BarLine.hair-thickness = 15 e'1
    R1 \override Score.BarLine.hair-thickness = 20 e'1
    R1 \override Score.BarLine.hair-thickness = 25 e'1
  }
  \addlyrics {
    Worked twice...
    and then
    I continued...
    working... correctly.
  }
  \new Staff { R1*16 }
>>
