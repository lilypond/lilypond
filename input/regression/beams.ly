
\version "2.3.16"
\header {
    texidoc = "Beaming can be also given explicitly."
}

    \paper { raggedright= ##t }

\score { 
\relative c'
	 {
	 c16[ c8.]
	 c8.[ c16]
	 c8[ c16 c16 c8]
	 c8[ c16 e16 g8]

    }}
