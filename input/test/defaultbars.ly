\version "1.3.146"


\header {

    texidoc = "By setting barAlways and defaultBarType, you can
    automatically insert barlines everywhere." 
}


\score {
	\notes {
		\property Score.barAlways = ##t
		\property Score.defaultBarType = ":|:"
		c4 c4 c4 c4 }
}
