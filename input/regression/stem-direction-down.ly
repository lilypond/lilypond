\version "1.5.68"

\header{

texidoc="Similarly, if @code{'neutral-direction} is set to @code{-1}.  "

}

\score{
	\notes\relative c{
	\property Voice.Stem \override #'neutral-direction = #-1
		 
	    b''4 ~ b8()b8 e4 e,
	}
	\paper{

	}
}
