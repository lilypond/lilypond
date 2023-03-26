\version "2.25.4"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="A final volta bracket overhanging the next section can be
achieved by overriding @code{Score.VoltaBracket.musical-length} in a
zero-duration alternative.  The context property
@code{voltaSpannerDuration} is ignored.  The bracket for volta 2
should end 1/3 of the way into the final measure."
}

music = \context Voice \fixed c' {
  \set Score.voltaSpannerDuration = \musicLength 4
  \repeat volta 2 {
    s1_"A"
    \alternative {
      s1_"B"
      \once \override Score.VoltaBracket.musical-length = \musicLength 1*4/3
    }
  }
  %% This music calls for a timestep where voltaSpannerDuration would
  %% take effect if musical-length were not set.
  s4_"C" %{ engraver ignores voltaSpannerDuration here %} s2.
  %% This music does not call for a timestep where the bracket is
  %% supposed to end.
  s1_"D"
}

\score { \music }
\score { \unfoldRepeats \music }
