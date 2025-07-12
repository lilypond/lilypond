\version "2.25.27"

\header {
  texidoc = "This test sets @code{beamExceptions} and @code{subdivideBeams} so
that beams with a value shorter than the beat break at the beat, but others span
the full measure."
}

\paper {
  indent = 0
  ragged-right = ##t
}

\layout {
  \context {
    \Staff
    \omit Clef
  }
}

%% Issue 6828: There are inconsistencies in the defaults for 3/8 and other 3/n.
%% This test is likely to deserve changes when the issue is resolved; the
%% property checks here should draw attention to that.

\relative c' {
  \time 3/8
  \contextPropertyCheck Timing.beamExceptions #'((end . ((1/8 . (3)))))
  \contextPropertyCheck Timing.subdivideBeams ##f
  \set Timing.subdivideBeams = ##t
  \repeat unfold 12 c32
}

\relative c' {
  \time 3/16
  \contextPropertyCheck Timing.beamExceptions #'()
  \contextPropertyCheck Timing.subdivideBeams ##f
  \set Timing.beamExceptions = #'((end . ((1/8 . (3/2)))))
  \set Timing.subdivideBeams = ##t
  \repeat unfold 12 c64
  c8 c16
  c16 c8
}

\relative c' {
  \time 3/32
  \contextPropertyCheck Timing.beamExceptions #'()
  \contextPropertyCheck Timing.subdivideBeams ##f
  \set Timing.beamExceptions = #'((end . ((1/8 . (3/4)))))
  \set Timing.subdivideBeams = ##t
  \repeat unfold 12 c128
  c16 c32
  c32 c16
}
