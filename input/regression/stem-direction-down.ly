\version "1.3.146"

\header{

texidoc="Similarly, if @code{'neutral-direction} is set to @code{-1}.  "

}

\score{
	\notes\relative c{
	\property Voice.Stem \override #'default-neutral-direction = #-1
		 
	    b''4 ~ b8()b8 e4 e,
	}
	\paper{

	}
}
