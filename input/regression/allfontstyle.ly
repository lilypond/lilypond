\version "1.9.2"

\header{
texidoc="
Different text styles are used for various purposes.
"
}

\score {  \notes
	\relative c'' \context Staff {
		\emptyText
		\repeat volta 2 { \time 4/4 c4^"cuivr\\'e"_\fermata }
		 \alternative {
		 	{
			    d-4_\markup { \italic "cantabile"  } }
		 	{  e }  } \accacciatura { c16 }
			 
			 f4\ff^""^\markup  { \large "Largo" } \mark "B" g 
	}
	\paper { raggedright = ##t
	% \translator {\BarNumberingStaffContext} }
	}
}
