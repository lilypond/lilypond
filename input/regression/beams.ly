
\version "2.4.0"
\header {
    texidoc = "Beaming can be also given explicitly."
}

    \layout { raggedright= ##t }

\score { 
\relative c'
	 {
	 c16[ c8.]
	 c8.[ c16]
	 c8[ c16 c16 c8]
	 c8[ c16 e16 g8]

    }}
