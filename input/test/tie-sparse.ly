
\version "2.1.26"

\header { texidoc = "@cindex Tie Sparse
Setting @code{sparseTies} generated only one tie per chord pair. "
}

% seem not to work -HJJ
	
\score { 
  \context Voice \notes\relative c {
	\set sparseTies = ##t
	c''  <c e g> ~ <c e g> 
  }
  \paper { raggedright = ##t }  
}

