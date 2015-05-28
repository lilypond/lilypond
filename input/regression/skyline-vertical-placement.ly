\header {
  texidoc = "Grobs that have outside-staff-priority set are positioned
using a skyline algorithm so that they don't collide with other objects."
}

\layout {ragged-right = ##t}

\version "2.19.21"
\relative {
  \override Score.PaperColumn.keep-inside-line = ##f
  \override TextScript.outside-staff-priority = #2
  \override DynamicLineSpanner.outside-staff-priority = #1
  c'''
  \once \override TextScript.self-alignment-X = #CENTER
  a,^"this doesn't collide with the c"
  b^"this goes above the previous markup"
  a8_"this goes below the dynamic"
  a\f
}
