\version "1.9.1"
% TODO: move stuff from ancient-font into here?  See comment
% for ancient-font.ly
\header {
texidoc="@cindex Ancient Time Signatures
Should use old style.
"
}

\score {
  \notes { 
    \property Staff.TimeSignature \override #'style = #'neo_mensural
    s1 
  }
	\paper {raggedright = ##t}
}

