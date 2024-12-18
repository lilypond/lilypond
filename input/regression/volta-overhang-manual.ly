\version "2.25.23"

\header {
  texidoc="A final volta bracket overhanging the next section can be achieved
manually with the @code{repeatCommands} property and the deprecated
@code{voltaSpannerDuration} property."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (G_ "'voltaSpannerDuration' is deprecated; \
use the grob property VoltaBracket.musical-length"))

music = \context Voice \fixed c' {
  \repeat volta 2 {
    s1_"A"
  } \alternative {
    s1_"B"
    {
      \volta #'() { % remove when unfolded
        \set Score.repeatCommands = #`(end-repeat
                                       start-repeat
                                       (volta ,#{ \markup \volta-number "2." #}))
        \set Score.voltaSpannerDuration = 1
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
