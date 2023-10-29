\version "2.25.10"

\header {
  texidoc = "If different @code{BarLine} types are used at the same musical
moment, setting @code{BarLine.right-justified} to @code{#t} right-aligns them.
For a mid-line and right-aligned @code{BarLine} the anchor moves accordingly.
At begin of line, @code{BarLine} is never right-aligned."
}

#(define-bar-line "|-alt" #t #t #t)
#(define-bar-line "|.|-alt" #t #t #t)
#(define-bar-line "..-alt" #t #t #t)

control = {
  s1
  \mark \default %% A
  \textMark "justified #f"
  s
  \break
  \mark \default  %% B
  \textEndMark "justified #t"
  \textMark "justified #t"
  \once \override Score.BarLine.right-justified = ##t
  s
  \mark \default %% C
  \textMark "justified #f"
  s
  \mark \default %% D
  \once \override Score.BarLine.right-justified = ##t
  \textMark "justified #t"
  s
  \mark \default %% E
  \once \override Score.BarLine.right-justified = ##t
  \textEndMark "justified #t"
}

\score {
  \new StaffGroup
    <<
      \new Staff \with { measureBarType = ":|." }
        { b1 b }

      \new Staff \with { measureBarType = "..-alt" }
        { b1 b b b }

      \new Staff \with { measureBarType = "|-alt" }
        << { b1 b b b b } \control >>

      \new Staff \with { measureBarType = "|.|-alt" }
        { b1 b b b b }

      \new Staff \with { measureBarType = "S-||" }
        { b1 b b b \set Staff.measureBarType = "|." }
    >>
  \layout {
    \context {
      \Score
      \override RehearsalMark.break-visibility = ##(#t #t #t)
      \override TextMark.direction = #DOWN
      \override SpanBar.color = #(make-list 3 0.7)
      \override SpanBar.glyph-name = "!"
      \override BarNumber.break-visibility = ##(#t #t #t)
    }
    \context {
      \Staff
      \override BarLine.color =
      #(lambda (grob)
        (when (ly:grob-property grob 'right-justified #f) '(0.7 0 0)))
    }
  }
}
