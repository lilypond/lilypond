\version "2.25.80"

\header {
  texidoc = "It is possible to override @code{TimeSignature@/.time-signature} to
change the printed time signature without changing related context properties.

The marginal labels show the values of the @code{style} and
@code{denominator-@/style} properties in each case."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  indent = 2 \cm

  \context {
    \Staff
    \remove Clef_engraver
  }
}

%% Covering the deprecated 'fraction property is a separate concern, but this
%% is a good place for it.
#(ly:expect-warning
  (G_ "the grob property '~a' is deprecated; use '~a'")
  "fraction"
  "time-signature")
#(define (expect-fraction expected)
  (lambda (grob org cur)
   (let ((actual (ly:grob-property grob 'fraction *unspecified*)))
    (if (not (equal? actual expected))
     (ly:input-warning (*location*) "Unexpected fraction:
  expected: ~a
  actual  : ~a" expected actual)))))

music = \fixed c' {
  \once \override Timing.TimeSignature.time-signature = 2/2
  \applyOutput Timing.TimeSignature #(expect-fraction '(2 . 2))
  \time 2/4
  s2

  \once \override Timing.TimeSignature.time-signature = #'(0 . 16/127)
  \applyOutput Timing.TimeSignature #(expect-fraction '(0 . 16/127))
  \time 2/4
  s2

  \once \override Timing.TimeSignature.time-signature = #'(3.14 . -4)
  \applyOutput Timing.TimeSignature #(expect-fraction '(3.14 . -4))
  \time 2/4
  s2

  \once \override Timing.TimeSignature.time-signature = #'(2/3 . 0)
  \applyOutput Timing.TimeSignature #(expect-fraction '(2/3 . 0))
  \time 2/4
  s2

  \once \override Timing.TimeSignature.time-signature = #'((2 3) . 8)
  \applyOutput Timing.TimeSignature #(expect-fraction '(5 . 8))
  \time 2/4
  s2

  \once \override Timing.TimeSignature.time-signature = #'((1 . 2) (3 . 4))
  \applyOutput Timing.TimeSignature #(expect-fraction '(5 . 4))
  \time 2/4
  s2
}

<<
  \new Staff \with {
    \override TimeSignature.style = #'C
    instrumentName = \markup \column {
      \typewriter "C"
      "(default)"
    }
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'numbered
    instrumentName = \markup \column {
      \typewriter "numbered"
      "(default)"
    }
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'numbered
    \override TimeSignature.denominator-style = #'none
    instrumentName = \markup \column {
      \typewriter "numbered"
      \typewriter "none"
    }
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'numbered
    \override TimeSignature.denominator-style = #'note
    instrumentName = \markup \column {
      \typewriter "numbered"
      \typewriter "note"
    }
  } \music

  \new Staff \with {
    \override TimeSignature.style = #'single-number
    instrumentName = \markup \column {
      \typewriter "single-number"
      "(default)"
    }
  } \music
>>
