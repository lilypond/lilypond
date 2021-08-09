\version "2.17.11"

\header {
  texidoc = "If @code{space-to-barline} is false, we measure the space
between the note and the start of the clef.  If @code{space-to-barline}
is true, we measure the space between the note and the start of the bar
line."
}

\paper {ragged-right = ##t}

{
  \override Score.SpacingSpanner.common-shortest-duration =
    #(ly:make-moment 1/16)
  c'2 \clef bass c'2 \clef treble
  \override NoteSpacing.space-to-barline = ##f
  c'2 \clef bass c'2 \clef treble

  % the following two measures should be spaced identically
  \override NoteSpacing.space-to-barline = ##t
  c'2 c'2
  \override NoteSpacing.space-to-barline = ##f
  c'2 c'2
  c'1
}
