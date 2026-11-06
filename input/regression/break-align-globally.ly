\version "2.27.5"

\header {
  texidoc = "@code{break-align-globally} takes a grob, or all break-aligned
grobs inside an axis group, out of the per-axis spacing of
@code{BreakAlignment}: they are spaced against the grobs of all axes as if
they were part of each of them.  In the first two scores, the key
cancellation in the upper staff and the key signature in the lower staff
share horizontal space by default, but not when the property is set for the
key signature.  In the last three scores, the time signature of a separate
@code{TimeSignatureContainer} context is placed after the change clef of the
upper staff, and shares horizontal space with it by default.  Setting the
property for the whole @code{VerticalAxisGroup} of the container, or just for
its @code{TimeSignature}, keeps them apart."
}

\layout {
  ragged-right = ##t
}

%% Two staves where only the upper one has a key cancellation and only the
%% lower one has a key signature.

\score {
  <<
    \new Staff {
      <>^"default: key cancellation and key signature share space"
      \key g \major
      g'1 |
      \key c \major
      \time 3/4
      c'2.
    }
    \new Staff {
      \key c \major
      d'1 |
      \key d \major
      \time 3/4
      d'2.
    }
  >>
  \layout { }
}

\score {
  <<
    \new Staff {
      <>^\markup \left-column {
	\typewriter "KeySignature.break-align-globally = ##t"
	"key cancellation and key signature are kept apart"
      }
      \key g \major
      g'1 |
      \key c \major
      \time 3/4
      c'2.
    }
    \new Staff \with {
      \override KeySignature.break-align-globally = ##t
    } {
      \key c \major
      d'1 |
      \key d \major
      \time 3/4
      d'2.
    }
  >>
  \layout { }
}

%% A time signature in its own context with its own axis group between two
%% staves, placed after the change clef of the upper staff.

\layout {
  \context {
    \Score
    \numericTimeSignature
    \breakAlignInsert #center-visible clef after time-signature
    \override TimeSignature.space-alist.clef = #'(extra-space . 1.0)
  }
  \context {
    \Staff
    \remove Time_signature_engraver
  }
  \context {
    \name TimeSignatureContainer
    \type Engraver_group
    \consists Axis_group_engraver
    \consists Time_signature_engraver
    \override VerticalAxisGroup.staff-affinity = #CENTER
    \override VerticalAxisGroup.nonstaff-relatedstaff-spacing =
    #'((minimum-distance . 7)
       (padding . 0.5))
  }
  \context {
    \StaffGroup
    \accepts TimeSignatureContainer
  }
}

\score {
  \new PianoStaff <<
    \new Staff {
      <>^"default: time signature and clefs share space"
      c'1 |
      \clef bass
      c2.
    }
    \new TimeSignatureContainer {
      s1 |
      \time 3/4
      s2.
    }
    \new Staff {
      \clef bass
      c1 |
      c2.
    }
  >>
}

\score {
  \new PianoStaff <<
    \new Staff {
      <>^\markup \left-column {
	\small \typewriter "VerticalAxisGroup.break-align-globally = ##t"
	"time signature and clefs are kept apart"
      }
      c'1 |
      \clef bass
      c2.
    }
    \new TimeSignatureContainer \with {
      \override VerticalAxisGroup.break-align-globally = ##t
    } {
      s1 |
      \time 3/4
      s2.
    }
    \new Staff {
      \clef bass
      c1 |
      c2.
    }
  >>
}

\score {
  \new PianoStaff <<
    \new Staff {
      <>^\markup \left-column {
	\typewriter "TimeSignature.break-align-globally = ##t"
	"time signature and clefs are kept apart" }
      c'1 |
      \clef bass
      c2.
    }
    \new TimeSignatureContainer \with {
      \override TimeSignature.break-align-globally = ##t
    } {
      s1 |
      \time 3/4
      s2.
    }
    \new Staff {
      \clef bass
      c1 |
      c2.
    }
  >>
}
