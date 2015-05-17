\version "2.19.21"

\header {

texidoc = "Ordering of the fingerings depends on vertical ordering of the notes, and  
is independent of up/@/down direction."

}

\paper { ragged-right = ##t }

\relative {
	%% input order is not 1 2 3 , output is.
	<c'-1 g'-3 e-2  b'-4 d-5 f-6 a-7 c-8 > 4

	\relative {
	  <a'^1 cis^3 e^5>
	  <a_1 cis_3 e_5>
	}
}
