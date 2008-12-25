\header {
  
  texidoc = "With @code{full-length-to-extent}, the extent of the
  attaching column for a full-length tuplet bracket can be ignored."

}
\version "2.12.0"

\new Staff {
   \set tupletFullLength = ##t
   
   \time 1/8
   \times 2/3 { c'16 c'16 c'16 }
   \times 2/3 { c'16 c'16 c'16 }
   \override TupletBracket #'full-length-to-extent = ##f
   \times 2/3 { c'16 c'16 c'16 }
   \override Score.RehearsalMark #'break-visibility = ##(#t #t #t)
   \override Score.RehearsalMark #'direction = #down
   \mark "xxxxxxxxxxxxxxxxxxxxxxx"
}
