
\version "2.1.36"
\header {
    texidoc = "Beaming can be also given explicitly."
}

    \paper { raggedright= ##t }

\score { \notes
\relative c'
	 {
	 c16[ c8.]
	 c8.[ c16]
	 c8[ c16 c16 c8]
	 c8[ c16 e16 g8]

    }}
