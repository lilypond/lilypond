\version "2.23.5"

\header {
  texidoc = "Show the effect of the @code{Beat_performer} on drum
tremolos: start of the bar and its beats are marked
by  @code{\\marcato} and @code{\\accent}, respectively, unless manual
syncopes in less distance than the last `regular' beat precede,
indicated with one of those two articulations explicitly."
}

"\*" = \repeat unfold \etc

music = \drummode {
  \* 32 sn32 | 1 |
  8 \* 3 { <>-> \* 8 32 } <>->
  \* #(+ 4 32) 32 | 1 |
  \bar "|."
}

\score {
  <<
    \new DrumStaff
    \new DrumVoice {
      <>^\markup { Without \typewriter { Beat_performer } }
      \music
    }
    \new DrumStaff
    \new DrumVoice \with {
      %% Instantiate Beat_performer also as an engraver to see its effects.
      \consists "Beat_performer"
      \consists "Beat_engraver"
    }
    {
      <>^\markup { With \typewriter { Beat_performer } }
      \music
    }
  >>
  \midi {
    \tempo 4=80
  }
  \layout {
  }
}
