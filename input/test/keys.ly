\version "1.3.42";

\score {
  \notes \relative c''
  {
	\property Staff. createKeyOnClefChange = ##t  
    \key bes; c2 \key c \minor;  c2
    \break
    \key bes \major; c2 \clef alto; c2   \key d; c1
  }
}
