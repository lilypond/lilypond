\version "2.23.8"

\header {
  lsrtags = "contemporary-notation, percussion, real-music, really-cool, rhythms"

  texidoc = "
In the following snippet, two parts have a completely different time
signature, yet remain synchronized.

The bar lines can no longer be printed at the @code{Score} level; to
allow independent bar lines in each part, the @code{Timing_translator}
is moved from the @code{Score} context to the @code{Staff} context.

If bar numbers are required, the @code{Bar_number_engraver} should also
be moved, since it relies on properties set by the
@code{Timing_translator}; a @code{\\with} block can be used to add bar
numbers to the relevant staff.
"

  doctitle = "Printing music with different time signatures"
}


\paper {
  indent = #0
  ragged-right = ##t
}

global = { \time 3/4 { s2.*3 } \bar "" \break { s2.*3 } }

\layout {
  \context {
    \Score
    \remove "Timing_translator"
    \remove "Bar_number_engraver"
    \override SpacingSpanner.uniform-stretching = ##t
    \override SpacingSpanner.strict-note-spacing = ##t
    proportionalNotationDuration = #(ly:make-moment 1/64)
  }
  \context {
    \Staff
    \consists "Timing_translator"
  }
  \context {
    \Voice
    \remove "Forbid_line_break_engraver"
    tupletFullLength = ##t
  }
}

Bassklarinette = \new Staff \with {
  \consists "Bar_number_engraver"
  barNumberVisibility = #(every-nth-bar-number-visible 2)
  \override BarNumber.break-visibility = #end-of-line-invisible
} <<
  \global {
    \bar "|"
    \clef treble
    \time 3/8
    d''4.

    \bar "|"
    \time 3/4
    r8 des''2( c''8)

    \bar "|"
    \time 7/8
    r4. ees''2 ~

    \bar "|"
    \time 2/4
    \tupletUp
    \tuplet 3/2 { ees''4 r4 d''4 ~ }

    \bar "|"
    \time 3/8
    \tupletUp
    \tuplet 4/3 { d''4 r4 }

    \bar "|"
    \time 2/4
    e''2

    \bar "|"
    \time 3/8
    es''4.

    \bar "|"
    \time 3/4
    r8 d''2 r8
    \bar "|"
  }
>>

Perkussion = \new StaffGroup <<
  \new Staff <<
    \global {
      \bar "|"
      \clef percussion
      \time 3/4
      r4 c'2 ~

      \bar "|"
      c'2.

      \bar "|"
      R2.

      \bar "|"
      r2 g'4 ~

      \bar "|"
      g'2. ~

      \bar "|"
      g'2.
    }
  >>
  \new Staff <<
    \global {
      \bar "|"
      \clef percussion
      \time 3/4
      R2.

      \bar "|"
      g'2. ~

      \bar "|"
      g'2.

      \bar "|"
      r4 g'2 ~

      \bar "|"
      g'2 r4

      \bar "|"
      g'2.
    }
  >>
>>

\score {
  <<
    \Bassklarinette
    \Perkussion
  >>
}
