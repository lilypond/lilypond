\version "1.3.110";

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
