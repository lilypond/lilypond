
%{

 Example template for a SATB vocal  score.
 
%}

sopMusic = \notes  \relative c'' { c4 c [c8( )b] c4 }
sopWords = \lyrics { hi4 hi hi hi  }

altoMusic = \notes  \relative c' { e4 f d e }
altoWords =\lyrics { ha4 ha ha ha }

tenorMusic = \notes \relative c' { g4 a f g }
tenorWords = \lyrics { hu4 hu hu hu }

bassMusic = \notes \relative c { c4 c g c }
bassWords = \lyrics { ho4 ho ho ho }

\score { \notes
	  \context StaffGroup <
	      \property Score.automaticMelismata = ##t
	  \context Lyrics = sopLyrs { s1 }
	  \context Staff = women { s1 }
	  \context Lyrics = altoLyrs { s1 }
	  \context Lyrics = tenorLyrs { s1 }
	  \context Staff = men {\clef bass s1 }
	  \context Lyrics = bassLyrs { s1 }
	  \addlyrics
		\context Staff = women \context Voice = VA { \voiceOne \sopMusic }
		\context Lyrics = sopLyrs { \sopWords}
	  \addlyrics
		\context Staff = women \context Voice = VB { \voiceTwo \altoMusic }
		\context Lyrics = altoLyrs { \altoWords}
	  \addlyrics
		\context Staff = men \context Voice = VA { \voiceOne \tenorMusic }
		\context Lyrics = tenorLyrs { \tenorWords}
	  \addlyrics
		\context Staff = men  \context Voice = VB { \voiceTwo \bassMusic }
		\context Lyrics = bassLyrs { \bassWords}
	  
	  >
  \paper {
    \translator {

	% a little smaller so lyrics can be closer to the staff. 
	\StaffContext
	minimumVerticalExtent = #'(-3 . 3) 
    }
  }
}
