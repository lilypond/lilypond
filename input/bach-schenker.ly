% -*-coding: utf-8 -*-

\header{

 composer ="J.S. Bach"
 title = "Wenn wir in hoechsten Noten sein"
 subtitle = "Analysis from Gene Biringer's Schenker Text, Ex. 5-27"
%% "BWV641"
 enteredby = "Kris Shaffer"
 
}

%% See http://www.shaffermusic.com/doc/schenker/index.html for more information
%% 


\version "2.10.0"

staffPiano = \new PianoStaff {
  \set Score.timing = ##f
  \set PianoStaff.followVoice = ##t

  <<
    \new Staff {  % Right hand 
      \clef treble
      \key g \major
      \relative c'' {
	\override Staff.NoteCollision
	#'merge-differently-headed = ##t
	<<
	  {
	    \override Beam  #'positions = #'(8 . 8)
	    \override NoteHead #'transparent = ##t
	    s1 b8[^\markup {
	      \override #'(baseline-skip . 0.5)
	      \column { \small {^ 3} }
	    }
		  s4. s1 a8^\markup {
		    \override #'(baseline-skip . 0.5)
		    \column { \small {^ 2} }
		  } s4. s2 g8]^\markup {
		    \override #'(baseline-skip . 0.5)
		    \column {
		      \small {^ 1}
		    }
		  }
	    s4.
	    \revert Beam #'positions
	    \revert NoteHead #'transparent
	  } 
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \slurDown
	    \override Staff.Slur #'height-limit = #6
	    \slurDashed
	    s1 
	    \once \override Slur #'extra-offset = #'(1.25 . 0)
	    b2( s2
	    \revert Staff.Slur #'height-limit
	    \override NoteHead #'transparent = ##t
	    b4) s 
	    \revert NoteHead #'transparent
	    \override Staff.Slur #'height-limit = #3.25
	    \once \override Slur #'extra-offset = #'(.75 . 0)
	    \slurDashed
	    a2( s4
	    \override NoteHead #'transparent = ##t
	    a) 
	    \revert NoteHead #'transparent
	    g2 
	    \revert Stem #'transparent
	  }
	  \\
	  \override Staff.NoteCollision
	  #'merge-differently-headed = ##t
	  {
	    \override Beam #'positions = #'(4 . -3.25)
	    \stemUp 
	    g8[ s s4 s2 
		\stemDown 
		\override NoteHead #'transparent = ##t
		b8] s8
	    \revert NoteHead #'transparent
	    \override Beam #'positions = #'(3 . -2.75)
	    \stemUp
	    a8[ s s4
		\stemDown
		c8] s s2 s s
	    \revert Stem #'length
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \override NoteHead #'transparent = ##t
	    \override Stem #'length = #0
	    \phrasingSlurDown
	    \slurUp            
	    g4\( 
	      \revert NoteHead #'transparent
	      \once \override TextScript #'padding = #1.5
	      \once\override Slur #'extra-offset = #'(0 . -0.35)
	      fis(_\markup { \tiny N } g)\) \slurUp a(^\markup { \tiny P } b2)
	    b4(^\markup { \tiny P }
	    \stemUp 
	    \revert Stem #'transparent
	    \override Stem #'length = #10
	    c8[])( s 
	  \override Stem #'length = #14
	  b4) s s
	  \override Stem #'length = #0
	  \override Stem #'transparent = ##t
	  \phrasingSlurUp \slurDown 
	  \override Slur #'extra-offset = #'(0 . 0.35)
	  c4\(
	    \once \override TextScript #'padding = #1.25
	    b(_\markup { \tiny P } a)\) s2
	  \revert Stem #'length
	  \revert Slur #'extra-offset
	}
	 \\
	 {
	   \override Stem #'transparent = ##t
	   \override NoteHead #'transparent = ##t
	   \override Stem #'length = #0
	   s1 s4 e4 s 
	   \change Staff=LH
	   fis,4 s2	
	   \revert Stem #'transparent
	   \revert NoteHead #'transparent
	   \revert Stem #'length
	 }
	 \\
	 {
	   \override Stem #'transparent = ##t
	   \override NoteHead #'transparent = ##t
	   \override Stem #'length = #0
	   s1 s s2
	   fis'4 s
	   \change Staff=LH
	   g,4 s s2
	   \revert Stem #'transparent
	   \revert NoteHead #'transparent
	   \revert Stem #'length
	 }
       >>
      \bar "|."
    }
  }
   \new Staff {  % Left hand 			
     \clef bass
     \key g \major
     \relative c' {
       \override Staff.NoteCollision
       #'merge-differently-headed = ##t
       <<
	 {
	   \override Beam  #'positions = #'(-8 . -8)
	   \override NoteHead #'transparent = ##t
	   \stemDown
	   g8[_\markup { \bold I } s4. s1 s s2
	      d8_\markup { \bold V } s4.
	      g,8]_\markup { \bold I } s4.
	   \revert Beam #'positions
	   \revert NoteHead #'transparent
	 }
	 \\
	 {
	   \override Stem #'transparent = ##t
	   \stemDown
	   \override TextScript #'extra-offset = #'(-11.75 . -12.25)
	   g'2 s1 s s2 d2 g,2
	   \revert Stem #'transparent
	 }
	 \\
	 {
	   \override Stem #'transparent = ##t
	   \override NoteHead #'transparent = ##t
	   \override Stem #'length = #0
	   g'4
	   \revert NoteHead #'transparent
	   \slurDown
	   \once \override TextScript #'padding = #0.25
	   a4(^\markup { \tiny P } b)
	   \slurUp
	   fis4(^\markup { \tiny P } e)
	   \override NoteHead #'transparent = ##t
	   c4( 
	   \revert NoteHead #'transparent
	   d)^\markup { \tiny N }
	   \slurDown
	   \override NoteHead #'transparent = ##t
	   \once \override Slur #'extra-offset = #'(0 . 0.5)
	   fis,4(
	   \revert NoteHead #'transparent
	   \revert Stem #'transparent
	   \override Stem #'length = #12
	   \stemDown
	   g4) s
	   \override Stem #'length = #10
	   \once \override Slur #'extra-offset = #'(0 . 0.25)
	   c8( s
	   \override Stem #'transparent = ##t
	   \revert Stem #'length
	   a4)
	   \override NoteHead #'transparent = ##t
	   \slurUp
	   d4(
	   \revert NoteHead #'transparent
	   d,4) s2
	 }
	 \\
	 {
	   \override Stem #'transparent = ##t
	   \override NoteHead #'transparent = ##t
	   \slurUp
	   \override Staff.Slur #'height-limit = #3.5
	   g'4( s b) s2
	   \revert Staff.Slur #'height-limit
	   \revert Stem #'transparent
	   \revert NoteHead #'transparent
	   \override Beam #'positions = #'(-4 . 1)
	   \stemDown
	   c,8[ s s4
		\stemUp
		fis,8] s
	   \override Beam #'positions = #'(1 . -4)
	   g8[ s
	       \stemDown
	       b8] s
	   \revert Beam #'positions
	   \override Stem #'transparent = ##t
	   \override NoteHead #'transparent = ##t
	   \slurUp
	   c4( s d4) s s2
	 }
	 \\
	 {
	   \override Stem #'transparent = ##t
	   \override NoteHead #'transparent = ##t
	   \slurDown
	   \override Stem #'length = #0
	   \stemDown
	   \override Slur #'height-limit = #3
	   \once \override Slur #'extra-offset = #'(0 . 0.25)
	   g4( s2. e4) s2. s2 s1 s2
	   \revert Stem #'transparent
	   \revert NoteHead #'transparent
	   \revert Staff.Slur #'height-limit
	 }
	 \\
	 {
	   \override Stem #'transparent = ##t
	   \override NoteHead #'transparent = ##t
	   \slurDown \slurDashed	
	   \override Staff.Slur #'height-limit = #6.0
	   \override Slur #'extra-offset = #'(0.5 . -0.25)
	   \override Stem #'length = #0
	   g4( s2. s1 g,4) s s1 s2
	   \revert Stem #'transparent
	   \revert NoteHead #'transparent
	   \revert Staff.Slur #'height-limit
	 }
       >>
       \bar "|."
     }
   }
 >>
}



\score {
  <<
    \staffPiano
  >>
  
  \midi {
  }

  \layout  {
    indent = 0.0
    ragged-right = ##t
    \context { \Staff \remove "Time_signature_engraver" }
  }
}

\paper {
}

