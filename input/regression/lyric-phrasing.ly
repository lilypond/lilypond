\version "2.2.0"
\header {

texidoc = "
  Normally, the lyric is centered on the note head. However, on
  melismata, the text is left aligned on the left-side of the note head.

"
}


\score{
    <<
	\notes \relative c' \context Voice = "bla" {
	    \autoBeamOff
	    c4( c16 d c b)  c4
	    d16[ e f g]
	    
	}
	\lyricsto "bla" \context Lyrics \lyrics {
	    alllll __ tijd
	    izzz
	}
    >>
    \paper { raggedright = ##t }
}
