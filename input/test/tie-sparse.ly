\version "1.7.18"

\header { texidoc = "@cindex Tie Sparse
Setting sparseTies causes only one tie to be
generated per chord pair. "
}
	
\score { 
  \context Voice \notes\relative c {
	\property Voice.sparseTies = ##t
	c''  <<c e g>> ~ <<c e g>> 
  }
  \paper { raggedright = ##t }  
}

