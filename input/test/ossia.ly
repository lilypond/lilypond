\version "2.1.7"
% pretty much a duplicate of extra-staff.ly. candidate for merge or delete. -gp

\header { texidoc = "@cindex Ossia
Ossias present alternatives for a piece. They are not
really supported, but can be somewhat faked in lily. "
}

\score {
\notes\relative c'' { c1 c1 <<
	{\clef "treble" c1 c1 }
	\new Staff { c1 c1 }
	>>
}
	\paper {raggedright= ##t}
}

