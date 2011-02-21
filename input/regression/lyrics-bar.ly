\version "2.13.51"

\header{
texidoc="
Adding a @code{Bar_engraver} to the Lyrics context makes sure that
lyrics do not collide with barlines.
"
}

\layout {
    ragged-right = ##t
}

<<
    \new Staff {
	b1 \bar "|:" b1 \bar ":|"
    }
    \context Lyrics \with {
	\consists "Bar_engraver"
	\consists "Separating_line_group_engraver"
    } \lyricmode {
	  looooooooooooooooooooooooooooooooooong1 syllable
      }
    \lyrics {
	no Bar_Engraver_Bar_Engraver_Bar_Engraver
    }
    \new Staff {
	b1 b1
    }
>>
\layout {
  \context {
    \Lyrics
      \override VerticalAxisGroup #'nonstaff-nonstaff-spacing #'minimum-distance = #4
      \override LyricText #'Y-offset = #-0.7
      \override BarLine #'bar-extent = #'(-2 . 2)
  }
}

