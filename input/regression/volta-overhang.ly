\version "2.23.0"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="A final volta bracket overhanging the next section can be
achieved with the @code{repeatCommands} property."
}

music = \context Voice \fixed c' {
  \repeat volta 2 {
    s1_"A"
  } \alternative {
    s1_"B"
    {
      %% TODO: Invent a cleaner alternative to this repeatCommands
      %% hack, but probably keep using voltaSpannerDuration to control
      %% the bracket length.

      %% The situation is that this alternative has no duration, so
      %% the implied \volta 2 function tells the Volta_engraver that
      %% this alternative ends immediately after it begins, so the
      %% engraver does not engrave a bracket.

      %% A specialized solution in the Volta_engraver could allow the
      %% volta bracket of a final alternative to overhang subsequent
      %% music, but a more general solution that schedules spanner-end
      %% (and other) events at a future time (which the Volta_engraver
      %% would handle like any other event) might also be useful for
      %% other things.

      \volta #'() { % remove when unfolded
        \set Score.repeatCommands = #'(end-repeat start-repeat (volta "2."))
        \set Score.voltaSpannerDuration = #(ly:make-moment 1)
      }
    }
  }

  \repeat volta 2 {
    s1_"C" | s_"D" |
    \unset Score.voltaSpannerDuration
  } \alternative {
    s_"E"
    { s_"F" s_"G" }
  }
}

\score { \music }
\score { \unfoldRepeats \music }
