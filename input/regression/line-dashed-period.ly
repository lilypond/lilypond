\header {

  texidoc = "The period of a dashed line is adjusted such that it
starts and ends on a full dash. "

}

\version "2.17.15"

\layout {
  indent = 0.0\mm
  ragged-right = ##T
}

\relative c' <<
  \new Staff {
    \crescTextCresc
    c1_\< c c1\!
  }
  \new Staff {
    \override DynamicTextSpanner.dash-period = #3
    \override DynamicTextSpanner.dash-fraction = #0.3
    \crescTextCresc
    c1^\< c c1\!
  }
>>
