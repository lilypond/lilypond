
\version "2.1.23"
\header{
	texidoc="@cindex Beam Count

You can alter the number of stems in a beam.  Here we see
two sets of four 32nds joined as if they were 8th notes.

" }

fragment = \notes {
  #(override-auto-beam-setting '(end * * * *)  1 4)
  f32 g a b b a g f

  f32 g a b 
  \set stemRightBeamCount = #1  b
  \set stemLeftBeamCount = #1 a
   g f
}


\score {
  \notes\relative c \fragment
  \paper { raggedright = ##t}  
}

