\version "2.25.35"

\header {
  categories = "Contexts and engravers, Editorial annotations, Repeats,
                Staff notation"

  texidoc = "
This snippet demonstrates the use of the
@code{Measure_counter_engraver} to number groups of successive
measures. Any stretch of measures may be numbered, whether consisting
of repetitions or not.

The engraver must be added to the appropriate context. Here, a
@code{Staff} context is used; another possibility is a @code{Dynamics}
context.

The counter is begun with @code{\\startMeasureCount} and ended with
@code{\\stopMeasureCount}. Numbering will start by default with@tie{}1,
but this behavior may be modified by overriding the @code{count-from}
property.

When a measure extends across a line break, the number will appear
twice, the second time in parentheses.
"

  doctitle = "Measure counters"
} % begin verbatim


\layout {
  \context {
    \Staff
    \consists #Measure_counter_engraver
  }
}

\new Staff {
  \startMeasureCount
  \*7 { c'4 d' e' f' }
  \stopMeasureCount
  \bar "||"
  g'4 f' e' d'
  \override Staff.MeasureCounter.count-from = #2
  \startMeasureCount
  \*5 { g'4 f' e' d' }
  g'4 f'
  \bar ""
  \break
  e'4 d'
  \*7 { g'4 f' e' d' }
  \stopMeasureCount
}
