\version "2.15.35"

\header {
  texidoc = "Empty measures do not confuse @code{SpanBarStub}.
These lyrics should remain clear of the span bars.
"
}

\new StaffGroup <<
  \new Staff { \repeat unfold 8 { R1 e'1 } }
  \addlyrics {
    Worked twice...
    and then
    I continued...
    working... correctly.
  }
  \new Staff { R1*16 }
>>
