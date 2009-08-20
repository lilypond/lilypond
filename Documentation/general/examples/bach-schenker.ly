% -*-coding: utf-8 -*-

\header{
  composer = "J.S. Bach"
  title = "Wenn wir in höchsten Nöten sein"
  subtitle = "Analysis from Gene Biringer's Schenker Text, Ex. 5-27"
% "BWV641"
  enteredby = "Kris Shaffer"
}

%%
%% See http://www.shaffermusic.com/doc/schenker/index.html for more information
%%

I = \once \override NoteColumn #'ignore-collision = ##t

\version "2.12.0"

staffPiano = \new PianoStaff {
  \set Score.timing = ##f
  \set PianoStaff.followVoice = ##t

  <<
    \new Staff = "RH" {  % Right hand
      \clef treble
      \key g \major
      \relative c'' {
	\override Staff.NoteCollision
	#'merge-differently-headed = ##t
	<<
	  {
	    \override Beam #'positions = #'(8 . 8)
	    \override NoteHead #'transparent = ##t
	    \override NoteHead #'duration-log = #1
	    s1 b8[^\markup {
	      \override #'(baseline-skip . 0.5)
	      \column { \small {^ 3} }
	    }
	    s4. s1 a8^\markup {
	      \override #'(baseline-skip . 0.5)
	      \column { \small {^ 2} }
	    }
	    s4. s2 g8]^\markup {
	      \override #'(baseline-skip . 0.5)
	      \column { \small {^ 1} }
	    }
	    s4.
	    \revert Beam #'positions
	    \revert NoteHead #'transparent
	    \revert NoteHead #'duration-log
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    s1
	    \once \override Slur #'height-limit = #6
	    \once \override Slur #'extra-offset = #'(1.25 . 0)
	    \slurDashed
	    \I b2_( s2
	    \once \override NoteHead #'transparent = ##t
	    b4) s
	    \once \override Slur #'height-limit = #3.25
	    \once \override Slur #'extra-offset = #'(.75 . 0)
	    a2_( s4
	    \once \override NoteHead #'transparent = ##t
	    a)
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
	      \once \override NoteHead #'transparent = ##t
	      \I b8] s8
	    \override Beam #'positions = #'(3 . -2.25)
	    \stemUp
	    a8[ s s4
	      \stemDown
	      c8] s s2 s s
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \override Stem #'length = #0
	    g4_\( fis^(_\markup { \tiny N } g)\)
	    a^(^\markup { \tiny P } b2)
	    b4^(^\markup { \tiny P }
	    \stemUp
	    \revert Stem #'transparent
	    \override Stem #'length = #10
	    c8)^( s
	    \override Stem #'length = #14
	    b4) s s
	    \override Stem #'length = #0
	    \override Stem #'transparent = ##t
	    \once \override Slur #'extra-offset = #'(0 . 0.35)
	    c4^\( b_(_\markup { \tiny P } a)\) s2
	    \revert Stem #'length
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \override NoteHead #'transparent = ##t
	    \override Stem #'length = #0
	    s1 s4 e4 s
	    \change Staff = "LH"
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
	    \change Staff = "LH"
	    g,4 s s2
	    \revert Stem #'transparent
	    \revert NoteHead #'transparent
	    \revert Stem #'length
	  }
	>>
	\bar "|."
      }
    }

    \new Staff = "LH" {  % Left hand
      \clef bass
      \key g \major
      \relative c' {
	\override Staff.NoteCollision
	#'merge-differently-headed = ##t
	<<
	  {
	    \override Beam #'positions = #'(-8 . -8)
	    \override NoteHead #'transparent = ##t
	    \stemDown
	    \I g8[_\markup { \bold I } s4. s1 s s2
	      \I d8_\markup { \bold V } s4.
	      \I g,8]_\markup { \bold I } s4.
	    \revert Beam #'positions
	    \revert NoteHead #'transparent
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \stemDown
	    \override TextScript #'extra-offset = #'(-11.75 . -12.25)
	    \I g'2 s1 s s2 \I d2 g,2
	    \revert Stem #'transparent
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \once \override NoteHead #'transparent = ##t
	    \override Stem #'length = #0
	    g'4
	    \once \override TextScript #'padding = #0.25
	    a4_(^\markup { \tiny P } b)
	    fis4^(^\markup { \tiny P } e)
	    \once \override NoteHead #'transparent = ##t
	    \once \override Slur #'height-limit = #1.5
	    c4^(
	    d)^\markup { \tiny N }
	    \once \override NoteHead #'transparent = ##t
	    \once \override Slur #'extra-offset = #'(0 . 0.5)
	    \I fis,4_(
	    \revert Stem #'transparent
	    \override Stem #'length = #10
	    \stemDown
	    g4) s
	    \once \override Slur #'extra-offset = #'(0 . 0.25)
	    \I c8_( s
	    \override Stem #'transparent = ##t
	    \revert Stem #'length
	    a4)
	    \once \override NoteHead #'transparent = ##t
	    \I d4^( d,4) s2
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \override NoteHead #'transparent = ##t
	    \I g'4^( s b) s2
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
	    c4^( s d4) s s2
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \override NoteHead #'transparent = ##t
	    \override Stem #'length = #0
	    \stemDown
	    \once \override Slur #'height-limit = #3
	    \once \override Slur #'extra-offset = #'(0 . 0.25)
	    \I g4_( s2. e4) s2. s2 s1 s2
	    \revert Stem #'transparent
	    \revert NoteHead #'transparent
	  }
	  \\
	  {
	    \override Stem #'transparent = ##t
	    \override NoteHead #'transparent = ##t
	    \slurDashed
	    \once \override Slur #'height-limit = #6.0
	    \once \override Slur #'extra-offset = #'(0.5 . -0.25)
	    \override Stem #'length = #0
	    g4_( s2. s1 g,4) s s1 s2
	    \revert Stem #'transparent
	    \revert NoteHead #'transparent
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

%  \midi {
%  }

  \layout {
    indent = 0.0
    ragged-right = ##f
    \context { \Staff \remove "Time_signature_engraver" }
  }
}


\paper {
}
