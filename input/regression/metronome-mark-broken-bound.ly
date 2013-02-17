\version "2.17.11"

\header {
texidoc = "A @code{MetronomeMark}, @code{RehearsalMark} and @code{BarNumber}
should not effect the starting point of spanners.
"
}

<<
 \new Staff {
   e'1 \time 4/4 \break |
   \tempo \markup { "fooooo" } 4 = 90
   e'1 |
   e'1 |
 }

 \new Staff {
   \override Score.MetronomeMark.break-visibility = #all-visible
   \override TupletBracket.breakable = ##t
   \override Beam.breakable = ##t
   \override Glissando.breakable = ##t

   \ottava #1 \tuplet 1/1 { e'8\<\startTextSpan\startTrillSpan\glissando
     [ \override NoteColumn.glissando-skip = ##t\repeat unfold 22 e'8
       \revert NoteColumn.glissando-skip e'8\!\stopTextSpan\stopTrillSpan ] } |
 }
 \addlyrics { ah __ \repeat unfold 21 { \skip 4 } _ rrgh }
 \addlyrics { ah --  \repeat unfold 21 { \skip 4 } _ rrgh }
>>

\layout {
 \context {
   \Voice
   \remove "Forbid_line_break_engraver"
 }
}
