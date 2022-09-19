\version "2.23.14"

\header {
  texidoc = "When a group of break-aligned items agree on the position
of their own anchors with respect to their own extents, the
``average'' anchor of the group falls at that position with respect to
the extent of the group.  In this case, each rehearsal mark should
point to the stated point relative to the compound time signature."
}

\layout {
  \context {
    \Score
    %% Point rehearsal marks to time signatures.
    \override TextMark.break-align-symbols = #'(time-signature)
    \override TextMark.self-alignment-X = #CENTER
  }

  \enablePolymeter
}

#(define-markup-command (test-mark layout props arg)
  (markup?)
  (interpret-markup layout props
    #{
      \markup \column {
        \center-align #arg
        \center-align "↓"
      }
    #} ))

\fixed c' <<
  \new Staff {
    \override Score.TimeSignature.break-align-anchor-alignment = -1.5
    \compoundMeter 1,1,1 \textMark \markup \test-mark "-1.5"
    c1 \bar "|"

    \override Score.TimeSignature.break-align-anchor-alignment = #LEFT
    \compoundMeter 1,1,1 \textMark \markup \test-mark "-1.0" \partial 1*100
    c1 \bar "|"

    \override Score.TimeSignature.break-align-anchor-alignment = #CENTER
    \compoundMeter 1,1,1 \textMark \markup \test-mark "0" \partial 1*100
    c1 \bar "|"

    \override Score.TimeSignature.break-align-anchor-alignment = #RIGHT
    \compoundMeter 1,1,1 \textMark \markup \test-mark "+1.0" \partial 1*100
    c1 \bar "|"

    \override Score.TimeSignature.break-align-anchor-alignment = 1.5
    \compoundMeter 1,1,1 \textMark \markup \test-mark "+1.5" \partial 1*100
    c1 \bar "|"
  }

  \new Staff {
    \repeat unfold 5 {
      \time 1/1
      c1
    }
  }
>>
