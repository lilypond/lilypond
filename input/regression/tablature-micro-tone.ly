\version "2.19.31"

\header {
  texidoc = "The @code{TabStaff} will print micro-tones as mixed numbers of
fret-number and a fraction.
The context-property @code{supportNonIntegerFret} needs to be set @code{#t}
in @code{Score}-context.
@code{FretBoards} will print those micro-tones only if they can be found in the
chosen settings for @code{stringTunings}, otherwise a warning (surpressed here)
will be printed and an empty @code{FretBoard} returned.  Which should be the
case here for the first pitch: @code{gih}"
}

#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (_ "No string for pitch ~a (given frets ~a)") #{ gih #} '())

\layout {
  \context {
    \Score
    supportNonIntegerFret = ##t
  }
}

custom-tuning = \stringTuning <e, a, d ges beh eeh'>

mus = \relative {
  gih4
  eeses'
  eeseh
  ees
  eeh
  e
  eih
  eis
  eisih
  eisis
}

<<
  \new Staff << \clef "G_8" \mus >>
  \new FretBoards \with { stringTunings = \custom-tuning } \mus
  \new TabStaff \with { stringTunings = \custom-tuning } \mus
>>
