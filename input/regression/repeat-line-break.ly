\version "2.1.7"

\header{
texidoc="
Across linebreaks, the left edge of a first and second alternative
bracket should be equal.
"
}

    \paper { raggedright= ##t }

\score  {
\notes
\context Staff\relative c''  {
	\repeat "volta" 2 { c1 \break } \alternative { d e }
	c1
	\repeat "volta" 2 { c1 } \alternative { { d \break}  e }

}
}

