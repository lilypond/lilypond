#(ly:set-option 'old-relative)
\version "1.9.1"

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

