#(ly:set-option 'old-relative)
\version "1.9.0"

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
		 	{ \property Voice.TextScript \set #'font-style = #'italic d-4_"cantabile" }
		 	{  e }  } \grace { c16 }
			\property Voice.TextScript \set #'font-style = #'large
			 f4-\ff^""^"Largo" \mark "B" g 
	}
	\paper { raggedright = ##t
	% \translator {\BarNumberingStaffContext} }
	}
}
