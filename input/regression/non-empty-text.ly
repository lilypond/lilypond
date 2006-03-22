
\version "2.8.0"

\header{
  texidoc=" By default, text is set with empty horizontal dimensions.  
The boolean property @code{no-spacing-rods} in @code{TextScript} is used
to control the horizontal size of text.  " }


\relative c''{
  %% \emptyText
  %% short for \override TextScript  #'no-spacing-rods = ##t
  c2_"very wide and long text" c | \break
  %% short for \override TextScript  #'no-spacing-rods = ##f
  \fatText
  c_"very wide and long text" c
}
\layout {
  line-width = 3.0\cm
}


