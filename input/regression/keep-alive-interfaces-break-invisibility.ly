\version "2.25.18"

\header {
  texidoc = "Grobs attached to system bounds should only keep alive
the systems where they are visible. From issue #6519"
}

\layout {
  ragged-right = ##t
   \context {
     \Score
     keepAliveInterfaces = #'(measure-spanner-interface
                              mark-interface
                              rest-interface)
     \remove Mark_engraver
   }
   \context {
     \Staff
     \override VerticalAxisGroup.remove-empty = ##t
     \override VerticalAxisGroup.remove-first = ##t
     \consists Measure_spanner_engraver
     \consists Mark_engraver
   }
}

\new Staff {
   s1\startMeasureSpanner s1\stopMeasureSpanner
   \break
   s1_"staff should be removed this system"
}

\new Staff {
  s1_"staff should be removed this system"
  \break
  \mark\default
  s1
}

\new Staff {
   R1
   \set Staff.keepAliveInterfaces = #'()
   \break
   <>_"staff should be removed this system"
   s1 R1
}

\new Staff {
   \set Staff.keepAliveInterfaces = #'()
   <>_"staff should be removed this system"
   R1 s1
   \unset Staff.keepAliveInterfaces
   \break
   R1
}
