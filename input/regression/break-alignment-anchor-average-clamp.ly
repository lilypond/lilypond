\version "2.23.14"

\header {
  texidoc = "In this case, the compound time signature has a CENTER
(0) anchor point and the 1/1 time signature has a LEFT (1) anchor
point.  The midpoint of these is 0.75, but it is not used for the
``average'' anchor point of the group because it would fall outside
the range of anchor points that the isolated time signatures would
choose.  Instead, the average anchor point is the closer extreme of
that range, which is the center of the compound time signature.  The
arrow should point there."
}

\layout {
  \context {
    \Score
    %% Point rehearsal marks at the right side of time signatures by
    %% default.
    \override TextMark.break-align-symbols = #'(time-signature)
    \override TextMark.self-alignment-X = #CENTER
    \override TimeSignature.break-align-anchor-alignment = #RIGHT
  }

  \enablePolymeter
}

\fixed c' <<
  \new Staff \with { % here, point at the right side (almost)
    \override TimeSignature.break-align-anchor-alignment = #CENTER
  } {
    \compoundMeter 1,2,3,1 \textMark "↓"
    c1 \bar "|"
  }
  \new Staff {
    \time 1/1
    c1
  }
>>
