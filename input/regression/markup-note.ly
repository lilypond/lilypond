\header {

  texidoc = "The note markup function may be used to make metronome
 markings. It works for a variety of flag, dot and duration settings."
}
\version "2.12.0"

\relative c''
{
  c4^\markup {
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

  \override NoteHead #'style = #'triangle
  c4 a
} 
