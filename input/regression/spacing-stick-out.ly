
\header {

  texidoc = "Unless @code{allow-outside-line} is set for the relevant
  PaperColumn, LilyPond will space a line to prevent text sticking out
  of the right margin."

}

\version "2.6.0"

\layout { raggedright = ##t } 
\relative {
  c1 c1^"This is a really long text" c
}

