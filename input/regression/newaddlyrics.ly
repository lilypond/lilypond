\header {

    texidoc = "newlyrics, multiple stanzas, multiple lyric voices."

}
\layout { ragged-right = ##t }
<<
    \new Staff \relative {
	d''2 d c4 bes a2 \break
    }
    \addlyrics {
	My first Li -- ly song,
    }
    \addlyrics {
	Not much can go wrong!
    }
    \new Staff \relative {
	\clef bass
	d'2 d c4 bes a2 \break
    }
    \addlyrics {
	MY FIRST LI -- LY SONG,
    }
    \addlyrics {
	NOT MUCH CAN GO WRONG!
    }
>>

\version "2.19.21"
