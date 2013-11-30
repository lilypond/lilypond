\version "2.19.0"

\header{
texidoc="

If the @code{Note_heads_engraver} is replaced by the @code{Completion_heads_engraver},
long notes, longer than @code{measureLength}, are split into un-scaled notes,
even if the original note used a scale-factor.
@code{completionFactor} controls this behavior."
}

\layout { ragged-right= ##t }


\new Voice \with {
    \remove "Note_heads_engraver"
    \consists "Completion_heads_engraver"
} \relative c'{

  c\breve |
  c1*2 |
  c2*4 |
  c8*20 r2 \break
  \tuplet 3/2 { d1 d d }
  % \breve*2/3 is longer than a measure, but we want a tuplet, not repeats.
  \set completionFactor = ##f
  \tuplet 3/2 { e\breve e e }
  \set completionFactor = #2/3
  \tuplet 3/2 { e\breve e e }
}
