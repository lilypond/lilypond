\version "2.1.7"
\header {

    texidoc = "
 Example template for a SATB vocal  score.
" 
}

sopMusic = \notes  \relative c'' { c4 c c8[( b)] c4 }
sopWords = \lyrics { hi4 hi hi hi  }

altoMusic = \notes  \relative c' { e4 f d e }
altoWords =\lyrics { ha4 ha ha ha }

tenorMusic = \notes \relative c' { g4 a f g }
tenorWords = \lyrics { hu4 hu hu hu }

bassMusic = \notes \relative c { c4 c g c }
bassWords = \lyrics { ho4 ho ho ho }

\score { \notes
	  \context StaffGroup <<
	      \context LyricsVoice = sopranos { s1 }
	      \context Staff = women <<
		  \context Voice = sopranos { \voiceOne \sopMusic }
		  \context Voice = altos { \voiceTwo \altoMusic }
	      >>
	      \context LyricsVoice = altos { s1 }
	      \context LyricsVoice = tenors { s1 }
	      \context Staff = men <<
		  \clef bass
		  \context Voice = tenors { \voiceOne \tenorMusic }
		  \context Voice = basses { \voiceTwo \bassMusic }
	      >>
	      \context LyricsVoice = basses { s1 }

	      
	      \context LyricsVoice = sopranos \newaddlyrics sopranos \sopWords
	      \context LyricsVoice = altos \newaddlyrics altos \altoWords
	      \context LyricsVoice = tenors \newaddlyrics tenors \tenorWords
	      \context LyricsVoice = basses \newaddlyrics basses \bassWords
	  
	  >>
  \paper {
    \translator {

	% a little smaller so lyrics can be closer to the staff. 
	\StaffContext
	minimumVerticalExtent = #'(-3 . 3) 
    }
  }
}
