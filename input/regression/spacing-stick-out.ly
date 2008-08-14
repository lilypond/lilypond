
\header {

  texidoc = "If @code{keep-inside-line} is set for the relevant
  PaperColumn, LilyPond will space a line to prevent text sticking out
  of the right margin."

}

\version "2.11.51"

\layout { ragged-right = ##t } 

\relative {
  \override Score.PaperColumn #'keep-inside-line = ##t 
  c1 c1^"This is a really long text" c
}

