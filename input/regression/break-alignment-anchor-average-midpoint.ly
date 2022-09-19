\version "2.23.14"

\header {
  texidoc = "The ``average'' anchor of a diverse group of
break-aligned items depends on the range of the particular anchors,
but not on the number of items.  In this case, the arrows should
appear at the same horizontal position in both measures though the 1/1
time signature appears twice in one measure and only once in the
next."
}

\layout {
  \context {
    \Score
    %% Point rehearsal marks at the left side (almost) of time
    %% signatures by default.  Numbers are chosen to avoid LEFT,
    %% CENTER, and RIGHT anchor points in case there is special
    %% handling for those.
    \override TextMark.break-align-symbols = #'(time-signature)
    \override TextMark.self-alignment-X = #CENTER
    \override TimeSignature.break-align-anchor-alignment = #-0.75
  }

  \enablePolymeter
}

\fixed c' <<
  \new Staff \with { % here, point at the right side (almost)
    \override TimeSignature.break-align-anchor-alignment = #1.25
  } {
    \compoundMeter 1,2,3,1 \textMark "↓"
    c1 \bar "|"
    \compoundMeter 1,2,3,1 \textMark "↓" \partial 1*5
    c1 \bar "|"
  }
  \new Staff {
    \time 1/1
    c1
    \time 1/1
    c1
  }
  \new Staff {
    \time 1/1
    c1
    c1
  }
>>
