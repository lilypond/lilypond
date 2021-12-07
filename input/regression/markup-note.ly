\header {

  texidoc = "The note markup function may be used to make metronome
 markings. It works for a variety of flag, dot and duration settings."
}
\version "2.21.0"

mrkp =
\markup {
   \note {1} #UP
   \note {2} #UP
   \note {4} #UP
   \note {8} #UP
   \note {16} #UP
   \note {32} #UP
   \note {64} #UP

   \note {1} #DOWN
   \note {2} #DOWN
   \note {4} #DOWN
   \note {8} #DOWN
   \note {16} #DOWN
   \note {32} #DOWN
   \note {64} #DOWN

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
   { \note-by-number #2 #1 #UP
     \note-by-number #2 #1 #DOWN
   }
   \override #'(style . triangle)
   { \note-by-number #2 #1 #UP
     \note-by-number #2 #1 #DOWN
   }
}

\score {
  \relative
  {
    c''4^\markup {
            \column {
                    \mrkp
                    \override #'(style . mensural) \mrkp
                    \override #'(flag-style . modern-straight-flag) \mrkp
                    \override #'(flag-style . old-straight-flag) \mrkp
                    \override #'(flag-style . flat-flag) \mrkp
            }
    }
    \override NoteHead.style = #'triangle
    c4 a
  }
  \layout {
    \context {
      \Score
      \override PaperColumn.keep-inside-line = ##f
    }
  }
}
