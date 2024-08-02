\version "2.15.19"

\header {
  texidoc = "If a @code{Slur} or a @code{Glissando} follows a @code{Tie}, the
corresponding fret number is displayed in parentheses."
}


mus = {
  \time 3/2
  b2~ b( b')
  b2(~ b b')
  <b d'>~ q( <b' d''>)
  <b_~ d'> q( <b' d''>)
  <b d'^~> q( <b' d''>)
  b~ b\glissando b'
  b~\glissando \once \override NoteColumn.glissando-skip = ##t b b'
  <b d'>~ <b d'>\glissando <b' d''>
  <b_~ d'> <b d'>\glissando <b' d''>
  <b d'^~> <b d'>\glissando <b' d''>
}

\score {
  <<
    \new Staff \mus
    \new Lyrics
      \lyricmode {
        _ "(0)" _
        _ "/" _
        _ \markup \column  \parenthesize { 3 4 } _
        _ \markup \column  { 3 \parenthesize 4 } _
        _ \markup \column  { \parenthesize 3 4 } _
        _ "(0)" _
        _ "/" _
        _ \markup \column  \parenthesize { 3 4 } _
        _ \markup \column  { 3 \parenthesize 4 } _
        _ \markup \column  { \parenthesize 3 4 } _
      }
    \new TabStaff
      \with { instrumentName = "default" }
      \mus
    \new TabStaff
      \with { instrumentName = "hideSplitTiedTabNotes" \hideSplitTiedTabNotes }
      \mus
    \new TabStaff
      \with {
        instrumentName = "tabFullNotation"
        \hideSplitTiedTabNotes
        \tabFullNotation
      }
      \mus
  >>
  \layout {
    indent = 3\cm
    \context {
      \Lyrics
      \override LyricText.font-size = #-4
      \override LyricText.baseline-skip = 1
      instrumentName = \markup \fontsize #-2 "first two tabs should print:"
      \override VerticalAxisGroup.staff-affinity = #CENTER
    }
  }
}
