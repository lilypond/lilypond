\header {

  texidoc = "New sections for spacing can be started with
@code{\\newSpacingSection}. In this example, a section is started at
the 4/16, and a 16th in the second section takes as much space as a
8th in first section."

}

\paper
{
  ragged-right = ##t
  }
\version "2.19.21"
\relative {
  \time 2/4
  c'4 c8 c 
  c8 c c4 c16[ c c8] c4

  \newSpacingSection
  \time 4/16
  c8[ c16 c]
  c16[ c  c8]
} 
  

