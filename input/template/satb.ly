\version "2.3.4"
\header {

    texidoc = "
 Four-part (SATB) vocal score.
" 
}

sopMusic =   \relative c'' { c4 c c8[( b)] c4 }
sopWords = \lyrics { hi4 hi hi hi  }

altoMusic =   \relative c' { e4 f d e }
altoWords =\lyrics { ha4 ha ha ha }

tenorMusic =  \relative c' { g4 a f g }
tenorWords = \lyrics { hu4 hu hu hu }

bassMusic =  \relative c { c4 c g c }
bassWords = \lyrics { ho4 ho ho ho }

\score { 
	  \context StaffGroup <<
	      \context Lyrics = sopranos { s1 }
	      \context Staff = women <<
		  \context Voice = sopranos { \voiceOne \sopMusic }
		  \context Voice = altos { \voiceTwo \altoMusic }
	      >>
	      \context Lyrics = altos { s1 }
	      \context Lyrics = tenors { s1 }
	      \context Staff = men <<
		  \clef bass
		  \context Voice = tenors { \voiceOne \tenorMusic }
		  \context Voice = basses { \voiceTwo \bassMusic }
	      >>
	      \context Lyrics = basses { s1 }

	      
	      \context Lyrics = sopranos \lyricsto sopranos \sopWords
	      \context Lyrics = altos \lyricsto altos \altoWords
	      \context Lyrics = tenors \lyricsto tenors \tenorWords
	      \context Lyrics = basses \lyricsto basses \bassWords
	  
	  >>
  \paper {
    \context {

	% a little smaller so lyrics can be closer to the staff. 
	\Staff
	minimumVerticalExtent = #'(-3 . 3) 
    }
  }
}
