\header {

texidoc = "Lyric phrasing:

  Normally, the lyric is centered on the note head. However, on
  melismata, the text is left aligned on the left-side of the note head.

"
}


\score{
\context Staff  {
 \addlyrics
   \notes \relative c' \context Voice = "bla" {
       \autoBeamOff
       c4( c16 d c b)  c4
       d16[ e f g]
						 
   }
   \lyrics \context LyricsVoice = "bla-1" {
       al tijd
       izzz
   }
 
 }

\paper { raggedright = ##t

	 \translator {
	     \VoiceContext

	 }
     }
}

