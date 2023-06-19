\version "2.25.7"

\header {
  texidoc = "In the unexpected case that @code{\\partial} specifies a duration
shorter than the following note, a bar line still appears after the specified
duration.

The score in common time should have four bar lines.  The score in 3/4 time
should have six bar lines."
}

#(ly:set-option 'warning-as-error #t)

%% one measure after a partial measure
\new Score \with {
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #all-visible
} \fixed c' {
  \partial 2 c1.
  \partial 2 c1.
}

%% two measures after a partial measure
\new Score \with {
  barNumberVisibility = #(every-nth-bar-number-visible 1)
  \override BarNumber.break-visibility = #all-visible
} \fixed c' {
  \time 3/4
  \partial 4 c1..
  \partial 4 c1..
}
