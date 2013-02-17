\header {
  
  texidoc = "With @code{full-length-to-extent}, the extent of the
  attaching column for a full-length tuplet bracket can be ignored."

}
\version "2.17.11"

\new Staff {
   \set tupletFullLength = ##t
   
   \time 1/8
   \tuplet 3/2 { c'16 c'16 c'16 }
   \tuplet 3/2 { c'16 c'16 c'16 }
   \override TupletBracket.full-length-to-extent = ##f
   \tuplet 3/2 { c'16 c'16 c'16 }
   \override Score.RehearsalMark.break-visibility = ##(#t #t #t)
   \override Score.RehearsalMark.direction = #down
   \mark "xxxxxxxxxxxxxxxxxxxxxxx"
}
