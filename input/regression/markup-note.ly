\header {

  texidoc = "The note markup function may be used to make metronome
 markings. It works for a variety of flag, dot and duration settings."
}
\version "2.17.12"

mrkp =
\markup {
   \note #"1" #1
   \note #"2" #1
   \note #"4" #1
   \note #"8" #1
   \note #"16" #1
   \note #"32" #1
   \note #"64" #1

   \note #"1" #-1
   \note #"2" #-1
   \note #"4" #-1
   \note #"8" #-1
   \note #"16" #-1
   \note #"32" #-1
   \note #"64" #-1

   \note #"1." #-1
   \note #"2." #-1
   \note #"4." #-1
   \note #"8." #-1
   \note #"16." #-1
   \note #"32." #-1
   \note #"64." #-1

   \note #"1." #1
   \note #"2." #1
   \note #"4." #1
   \note #"8." #1
   \note #"16." #1
   \note #"32." #1
   \note #"64." #1

   \override #'(style . cross)
   { \note-by-number #2 #1 #1
     \note-by-number #2 #1 #-1
   }
   \override #'(style . triangle)
   { \note-by-number #2 #1 #1
     \note-by-number #2 #1 #-1
   }
}

\score {
  \relative c''
  {
    c4^\markup {
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
