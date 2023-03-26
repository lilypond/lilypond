\version "2.25.3"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc="A final volta bracket overhanging the next section can be
achieved manually with the @code{repeatCommands} and
@code{voltaSpannerDuration} properties."
}

music = \context Voice \fixed c' {
  \repeat volta 2 {
    s1_"A"
  } \alternative {
    s1_"B"
    {
      \volta #'() { % remove when unfolded
        \set Score.repeatCommands = #'(end-repeat start-repeat (volta "2."))
        \set Score.voltaSpannerDuration = \musicLength 1
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
