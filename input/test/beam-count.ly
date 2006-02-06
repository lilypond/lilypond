
%%\version "2.7.32"
\header{
	texidoc="@cindex Beam Count

You can alter the number of stems in a beam.  In this example,
two sets of four 32nds are joined, as if they were 8th notes.

" }

\layout { ragged-right = ##t}  

\relative {
  %% This has now (2.5.21) changed, (end * * * *) no longer
  %% masks the default config entry ('(end * * 2 4) 1 4))
  %% rather than masking by override:
  %% #(override-auto-beam-setting '(end * * * *) 1 4)
  %% revert the config file settings.
  #(revert-auto-beam-setting '(end 1 32 4 4) 1 8)
  #(revert-auto-beam-setting '(end 1 32 4 4) 3 8)
  f32 g a b b a g f

  f32 g a 
  \set stemRightBeamCount = #1  b
  \set stemLeftBeamCount = #1 b
  a g f
}
