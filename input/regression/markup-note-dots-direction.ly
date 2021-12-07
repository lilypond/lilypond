\header {
  texidoc = "In the @code{\\note} markup command, the position of dots
 may be changed."
}
\paper {
  #(set-paper-size "a3landscape")
}
\version "2.23.6"

mrkp =
\markup {
  \note {1.} #DOWN
  \note {2.} #DOWN
  \note {4.} #DOWN
  \note {8.} #DOWN
  \note {16.} #DOWN
  \note {32.} #DOWN
  \note {64.} #DOWN

  \note {1.} #UP
  \note {2.} #UP
  \note {4.} #UP
  \note {8.} #UP
  \note {16.} #UP
  \note {32.} #UP
  \note {64.} #UP

  \override #'(style . cross)
  {
    \note-by-number #2 #1 #UP
    \note-by-number #2 #1 #DOWN
  }
  \override #'(style . triangle)
  {
    \note-by-number #2 #1 #UP
    \note-by-number #2 #1 #DOWN
  }
}

mrkp-styles =
\markup \column {
  \mrkp
  \override #'(style . mensural) \mrkp
  \override #'(flag-style . modern-straight-flag) \mrkp
  \override #'(flag-style . old-straight-flag) \mrkp
  \override #'(flag-style . flat-flag) \mrkp
}

\markuplist
\override #'(padding . 5)
\table #`(,LEFT ,LEFT ,LEFT)
{
  "Default:" "Dots shifted up:" "Dots shifted down:"
  \mrkp-styles
  \override #`(dots-direction . ,UP) \mrkp-styles
  \override #`(dots-direction . ,DOWN) \mrkp-styles
}
