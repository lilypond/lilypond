\version "2.3.8"
\header{
    texidoc="

@cindex shorter volta bracket

By setting @code{voltaSpannerDuration}, the horizontal length of a volta 
bracket can be shortened.

"

}


\score {
    <<
	\context Staff \relative c''{
	    c c c c
				% coda-klugde: let volta span only one bar
	    \set Staff.voltaSpannerDuration = #(ly:make-moment 1 1)
	    \repeat "volta" 5 { d d d d }
	    \alternative { { e e e e f f f f }
			   { g g g g } }
	}
	\context Lyrics \lyrics{
	    intro1
	    \repeat fold 5 { }
	    \alternative {
		{ chorus1 one verse1 }
		{ chorus1 two verse1 }
		{ chorus1 three verse }
		{ chorus1 four verse }
	    }
	    five1
	}
    >>
    \paper{ raggedright = ##t }
}


				% 

