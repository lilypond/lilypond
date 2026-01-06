\version "2.25.32"

\header {
  categories = "Contemporary notation, Contexts and engravers, Rhythms,
                Workaround"

  texidoc = "
@subsubsubheading Flexible polymeter with unaligned measures

To support explicit creation of independently measured contexts, remove the
@code{Timing_translator} from @code{Score} context and define a
@code{TimingStaffGroup} context that has @code{Timing_translator}.  This makes
@code{Timing} an alias for @code{TimingStaffGroup}, targeting @code{\\time}
commands to the enclosing @code{TimingStaffGroup}.

Unlike LilyPond's built-in @code{\\enablePerStaffTiming} command, this approach
requires the explicit creation of @code{TimingStaffGroup} contexts; in exchange,
it allows creating multiple @code{Staff} contexts that jointly follow the
measure defined in their enclosing @code{TimingStaffGroup}.

@subsubsubheading Locally scaled time signatures

Use the unscalable @code{\\time} command to establish a measure of the desired
length in @code{Timing}, a.k.a. @code{TimingStaffGroup}.  In this snippet, all
staves below @code{TimingStaffGroup} use a scaled time signature, so any time
signature with the desired measure length is as good as any other.  If there
were an enclosed context that did not use a scaled time signature, the choice of
time signature to set in @code{Timing} would matter in that context.

Use the @code{\\polymetric \\time} command to set scalable metric properties in
contexts below @code{Timing}, and use the @code{\\scaleDurations} command to
scale both the local meter and the notes to fit the measure."

  doctitle = "Changing time signatures inside a polymetric section using \\scaleDurations"
}

\layout {
  \context {
    \Score
    \remove "Timing_translator"
    \accepts TimingStaffGroup
  }
  \context {
    \StaffGroup
    \name TimingStaffGroup
    \alias StaffGroup
    \consists "Timing_translator"
  }
}

<<
  \new TimingStaffGroup <<
    \new Staff {
      \scaleDurations 8/5 {
        \time 6/5 % to set measure length in Timing
        \context Staff \polymetric \time 6/8
        b8 b b b b b
        \time 4/5 % to set measure length in Timing
        \context Staff \polymetric \time 2/4
        b4 b
      }
    }
  >>
  \new TimingStaffGroup <<
    \new Staff {
      \clef bass
      \time 2/4
      c2 d e f
    }
  >>
>>
