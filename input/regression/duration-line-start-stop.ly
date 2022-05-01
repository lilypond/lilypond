\version "2.21.3"

\header {
  texidoc = "A @code{DurationLine} grob may start/end at @code{NoteHead},
@code{Rest}, @code{skip-event} (if forced, otherwise skips are passed),
@code{NoteHead}s of @code{EventChord} or at an entire @code{NoteColumn}.
Start/end at @code{MultiMeasureRest} is only basically supported.

It stops automatically if the @code{Voice} pauses, i.e., no rhythmical events
happen for some time, and at end of score.

Avoids @code{Dots} (if forced), @code{Accidental}s and @code{Arpeggio}
(per changeable default)."
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

skipUp = \markup \column { "↑" "skip" }
skipDown = \markup \column { "skip" "↓" }

{
  b4\- b
  r4\- r
  b4\- s_\skipUp s^\skipDown b
  \set startAtSkip = ##t
  \set endAtSkip = ##t
  s2_\skipUp\- s^\skipDown
  \unset startAtSkip
  \unset endAtSkip
  \once \override DurationLine.thickness = 3
  <g' c'' d''>\- <g' b' d''>
  \once \set startAtNoteColumn = ##t
  \once \override DurationLine.thickness = 25
  \once \override DurationLine.bound-details.left.padding = 0.2
  <g' c'' d''>\- <g' b' d''>
  R1\- R
  b''2\- \new Voice d'
  \once \override DurationLine.bound-details.left.start-at-dot = ##t
  <g' c'' d''>2.\- s4
  s2 <gis' cis'' dis''>\arpeggio
  \override DurationLine.bound-details.right.end-on-arpeggio = ##f
  <g' c'' d''>2\- <gis' cis'' dis''>\arpeggio
  \override DurationLine.bound-details.right.end-on-accidental = ##f
  <g' c'' d''>2\- <gis' cis'' dis''>\arpeggio
  \override DurationLine.bound-details.right.end-on-arpeggio = ##t
  <g' c'' d''>2\- <gis' cis'' dis''>\arpeggio
  b'1\-
  \bar "|."
}