\version "1.5.68"
\header {
texidoc = "slur or long note on other staves fool lily into extending melisma"
}
\score {
  <
    \addlyrics
      \notes {
	\property Staff.automaticMelismata= ##t
	\context Voice = melismaBla { c4 () c r c }
      }
      \context LyricsVoice = "melismaBla-1" \lyrics { foo __ bar }
    \context Staff=foolMelismata \notes{
      c4  c c  c
    }  
    \context Staff=OtherFoolMelismata \notes{
      c1
    }  
  >  
}
