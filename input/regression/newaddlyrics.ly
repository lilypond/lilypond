\header {
    
    texidoc = "newlyrics, multiple stanzas, multiple lyric voices."
    
}
\paper { raggedright = ##t }
<<
    \new Staff \relative {
	d'2 d c4 bes a2 \break
    }
    \newlyrics {
	My first Li -- ly song,
    }
    \newlyrics {
	Not much can go wrong!
    }
    \new Staff \relative {
	\clef bass
	d2 d c4 bes a2 \break
    }
    \newlyrics {
	MY FIRST LI -- LY SONG,
    }
    \newlyrics {
	NOT MUCH CAN GO WRONG!
    }
>>

\version "2.3.4"
