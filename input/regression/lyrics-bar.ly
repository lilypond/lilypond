\version "2.23.11"

\header{
texidoc="
Adding a @code{Bar_engraver} to the Lyrics context makes sure that
lyrics do not collide with bar lines.
"
}

\layout {
    ragged-right = ##t
}

\relative c'' <<
    \new Voice = "a"{
	b1 \bar ".|:-|" b1 \bar ":|." b1 \bar "|."
    }
    \new Lyrics \with {
	\consists "Bar_engraver"
	\consists "Separating_line_group_engraver"
    } \lyricsto "a" {
	  bars lengthened if
      }
    \new Lyrics \lyricsto "a" {
	required for noncollision
    }
    \new Staff {
	b1 b1 b1
    }
>>
\layout {
  \context {
    \Lyrics
      \override VerticalAxisGroup.nonstaff-nonstaff-spacing.minimum-distance = #4.2
      \override LyricText.Y-offset = #-0.7
      \override BarLine.bar-extent = #'(-2 . 2)
  }
}

