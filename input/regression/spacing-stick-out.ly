
\header {

  texidoc = "LilyPond will space a line to prevent text sticking out of the
  right margin unless @code{keep-inside-line} is false for the relevant
  PaperColumn."

}

\version "2.14.0"

\layout { ragged-right = ##t } 

\relative c' {
  c1 c1^"This is a really long text" c
}

