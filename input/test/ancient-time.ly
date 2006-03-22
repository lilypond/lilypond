\version "2.8.0"
% TODO: move stuff from ancient-font into here?  See comment
% for ancient-font.ly
\header {
texidoc="@cindex Ancient Time Signatures
Time signatures may also be engraved in an old style.
"
}

\score {
   { 
    \override Staff.TimeSignature  #'style = #'neomensural
    s1 
  }
	\layout {ragged-right = ##t}
}

