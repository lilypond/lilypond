\header{
texidoc="
Key signatures appear on key  changes. They may also
appear without barlines.  The restoration accidentals are not printed at
the start of the line. If @code{createKeyOnClefChange} is set, they're
also created on a clef change.
";
}



\score {
  \notes \relative c''
  {
	\property Staff. createKeyOnClefChange = ##t  
    \key bes \major; c2
%    \key c \major; %  \minor;
    \key es \major; %  \minor;
    c2
    \break
    \key bes \major; % \major;
    c2 \clef alto; c2   \key d \major; \clef treble; c2
  	\property Staff. keySignature = #'((2 . -1)  (6 . -1) (4 . -1))
	e2
  }
}
