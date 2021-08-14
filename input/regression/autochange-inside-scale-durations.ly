\version "2.23.4"

\header {
  texidoc = "Music functions that scale durations also scale
@code{\\autoChange} decisions.  The four measures should have
identical notes."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  ragged-right = ##t
  \context {
    \Voice
    %% This test focuses narrowly on scaling the auto-change timing
    %% data.  Avoid warnings due to the Tuplet_engraver's inability to
    %% follow the music as the auto-changer shifts it between
    %% contexts.
    \remove "Tuplet_engraver"
  }
}

acmus = \fixed c' \autoChange { c,2 \grace e'8 c,2 e' }

expected = \fixed c' \autoChange { c,2*2/3 \grace e'8*2/3 c,2*2/3 e'2*2/3 }

\new Score \fixed c' <<
  \context Staff = "up" { s1 }
  \context Staff = "down" {
    \clef "bass"
    \expected
    \scaleDurations 2/3 \acmus
    \times 2/3 \acmus
    \tuplet 3/2 \acmus
  }
>>
