\version "2.1.26"

\header {
texidoc = " Easy-notation (or Ez-notation) prints names in note heads.
You also get ledger lines, of course."
}

\score {
        \notes { c'2 e'4 f' | g'1 b8 }
        \paper { \translator { \EasyNotation }
		 raggedright = ##t
	     } 
}

