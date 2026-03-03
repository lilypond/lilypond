\version "2.25.35"

\header {
  texidoc = "The default metronome mark formatter rounds the rate according to
the @code{tempoCountPrecision} property.  Each staff should show the rate
66.66... rounded to the stated precision."
}

#(ly:set-option 'warning-as-error #t)

\paper {
  indent = 0
  ragged-right = ##t
}

\new Score \with {
  \remove Bar_number_engraver
} \fixed c' {
  \tempo "default" 4 = #200/3
  c1 \break

  \once \set Timing.tempoCountPrecision = #5
  \tempo "5" 4 = #200/3
  c1 \break

  \once \set Timing.tempoCountPrecision = #1
  \tempo "1" 4 = #200/3
  c1 \break

  \once \set Timing.tempoCountPrecision = #1/2
  \tempo "1/2" 4 = #200/3
  c1 \break

  \once \set Timing.tempoCountPrecision = #1/4
  \tempo "1/4" 4 = #200/3
  c1 \break

  \once \set Timing.tempoCountPrecision = #1/100
  \tempo "1/100" 4 = #200/3
  c1 \break

  \once \set Timing.tempoCountPrecision = #1/1000000000
  \tempo "1/1000000000" 4 = #200/3
  c1 \break
}
