
\version "2.1.22"

\header { texidoc = "@cindex Tie Sparse
Setting sparseTies causes only one tie to be
generated per chord pair. "
}
	
\score { 
  \context Voice \notes\relative c {
	\set sparseTies = ##t
	c''  <c e g> ~ <c e g> 
  }
  \paper { raggedright = ##t }  
}

