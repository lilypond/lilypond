
\version "2.3.8"
\header{
	texidoc="@cindex Beam Count

You can alter the number of stems in a beam.  In this example,
two sets of four 32nds are joined, as if they were 8th notes.

" }

\paper { raggedright = ##t}  

\relative {
  #(override-auto-beam-setting '(end * * * *)  1 4)
  f32 g a b b a g f

  f32 g a 
  \set stemRightBeamCount = #1  b
  \set stemLeftBeamCount = #1 b
  a g f
}
