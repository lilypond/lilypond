\version "2.21.3"

\header {
  texidoc = "Several styles for the @code{DurationLine} grob are available:
@code{'beam}, @code{'line}, @code{'dashed-line}, @code{'dotted-line},
@code{'zigzag}, @code{'trill} and @code{'none}."
}

\layout {
  \context {
    \Voice
    \consists "Duration_line_engraver"
    \omit Stem
    \omit Flag
    \omit Beam
    \override NoteHead.duration-log = 2
  }
}

{
  <>^"'beam"
  a'1\- s2 r

  <>^"'line"
  \once \override DurationLine.style = #'line
  a'1\- s2 r

  <>^"'dashed-line"
  \once \override DurationLine.style = #'dashed-line
  \once \override DurationLine.dash-period = 2
  a'1\- s2 r

  <>^"'dotted-line"
  \once \override DurationLine.style = #'dotted-line
  \once \override DurationLine.dash-period = 1
  \once \override DurationLine.bound-details.right.padding = 1
  a'1\- s2 r

  <>^"'zigzag"
  \once \override DurationLine.thickness = 2
  \once \override DurationLine.style = #'zigzag
  a'1\- s2 r

  <>^"'trill"
  \once \override DurationLine.style = #'trill
  a'1\- s2 r
  <>^"'none"
  \once \override DurationLine.style = #'none
  a'1\- s2 r
  \bar "|."
}