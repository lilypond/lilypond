\version "1.3.42";

\score {
  \notes \relative c''
  {
	\property Staff. createKeyOnClefChange = ##t  
    \key bes; c2
%    \key c ; %  \minor;
    \key es ; %  \minor;
    c2
    \break
    \key bes ; % \major;
    c2 \clef alto; c2   \key d; \clef treble; c2
  	\property Staff. keySignature = #'((2 . -1)  (6 . -1) (4 . -1))
	e2
  }
}
