\version "2.25.5"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "This tests polymetric staves beginning at different
times, with upbeats specified with @code{\\partial}.  One staff in 4/4
time and another in 3/4 time should end simultaneously.  A third staff
in 2/4 time should begin simultaneously with the staff in 3/4 time
(apart from its grace note) and end after 2@tie{}full measures in 3/4
time.  The ossia staves should have bar@tie{}1 after the upbeat, as
usual."
}

\layout {
  \enablePolymeter

  \context {
    \Staff
    \consists Bar_number_engraver
    \consists Staff_collecting_engraver
    \override BarNumber.break-visibility = #all-visible
    barNumberVisibility = #all-bar-numbers-visible
  }
  \context {
    \Score
    \remove Bar_number_engraver
    \remove Staff_collecting_engraver
  }
}

\fixed c' <<
  \new Staff = "A" {
    \time 4/4 g1 | a1 | b1 |
  }
  {
    \skip 2
    \new Staff = "B" {
      \time 3/4 \partial 4 d4 | e2. | f2. | g2. |
    }
  }
  {
    \skip 2
    \new Staff = "C" {
      \time 2/4 \partial 4 \grace d8 d4 c2 | d2 | e2 |
    }
  }
>>
