\version "2.19.31"

\header {
  texidoc = "The @code{TabStaff} will print micro-tones as mixed numbers of
fret-number and a fraction.
The context-property @code{supportNonIntegerFret} needs to be set @code{#t}
in @code{Score}-context.
@code{FretBoards} will print those micro-tones only if they can be found in the
chosen settings for @code{stringTunings}, otherwise a warning (surpressed here)
will be printed and an empty @code{FretBoard} returned.  Which should be the
case for the last four of the examples pitches.
Micro-tones assigned to strings work nicely."
}

#(ly:set-option 'warning-as-error #t)
#(for-each
  (lambda (pitch)
    (ly:expect-warning (G_ "No string for pitch "))
    (ly:expect-warning (G_ "Requested string for pitch requires negative fret"))
    (ly:expect-warning (G_ "Ignoring string request and recalculating.")))
  (iota 4))

\layout {
  \context {
    \Score
    supportNonIntegerFret = ##t
  }
}

custom-tuning = \stringTuning <e, a, d ges beh eeh'>

mus = \relative {
  eeses'4
  eeseh
  ees
  eeh
  e
  eih
  eis
  eisih
  eisis
  geseh,,\6
  geh\6
  gih\6
  gisih\6
}

<<
  \new Staff << \clef "G_8" \mus >>
  \new FretBoards \with { stringTunings = \custom-tuning } \mus
  \new TabStaff \with { stringTunings = \custom-tuning } \mus
>>
