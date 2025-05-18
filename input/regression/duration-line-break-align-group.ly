\version "2.25.27"

\header {
  texidoc = "
@code{DurationLine} avoids items of @code{BreakAlignGroup} at line-end, but not
mid-line.  The mid-line behaviour may be changed by enabling
@code{end-on-break-align-group}."
}


test =
<<
  \new Staff
    {
      g'1\- s1 \break s1 g'1\- s1 s1
      \break
      \bar "||"
      \override DurationLine.bound-details.right.end-on-break-align-group = ##t
      g'1\- s1 \break s1 g'1\- s1 s1
      \bar "|."
    }
  \new Staff
    {
      g'1\- s1 \break \clef alto r2 g2 g'1\- s1 \clef treble r2 g2
      \override DurationLine.bound-details.right.end-on-break-align-group = ##t
      g'1\- s1 \break \clef alto r2 g2 g'1\- s1 \clef treble r2 g2
    }
  \new Staff
    {
      g'1\- s1 \break \clef alto R1 g'1\- s1 \clef treble R1
      \override DurationLine.bound-details.right.end-on-break-align-group = ##t
      g'1\- s1 \break \clef alto R1 g'1\- s1 \clef treble R1
    }
>>

\score {
  \test
  \layout {
    \context {
      \Voice
      \consists Duration_line_engraver
    }
  }
}
