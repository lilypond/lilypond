% -*-coding: utf-8 -*-

\header{
  composer = "J.S. Bach"
  title = "Wenn wir in höchsten Nöten sein"
  subtitle = "Analysis from Gene Biringer's Schenker Text, Ex. 5-27"
% "BWV641"
  enteredby = "Kris Shaffer"
}

% See http://kris.shaffermusic.com/tech.html. for more information

% 'Add color...' sections are not the original author's, but added
% afterwards specifically for illustration in LilyPond's Documentation.

I = \once \override NoteColumn.ignore-collision = ##t

\version "2.17.6"

staffPiano = \new PianoStaff {
  \set Score.timing = ##f
  \set PianoStaff.followVoice = ##t
  <<
    \new Staff = "RH" { % Right hand
      \clef treble
      \key g \major
      \relative c'' {
	\override Staff.NoteCollision.merge-differently-headed = ##t
	<<
	  {
	    \override Beam.positions = #'(8 . 8)
	    \override NoteHead.transparent = ##t
	    \override NoteHead.duration-log = #1
	    s1 b8[^\markup {
	      \override #'(baseline-skip . 0.5)
              % Add color to markup in top staff
              \column { \with-color #red \small { ^ 3 } }
	    }
	    s4. s1 a8^\markup {
	      \override #'(baseline-skip . 0.5)
              % Add color to markup in top staff
              \column { \with-color #red \small { ^ 2 } }
	    }
	    s4. s2 g8]^\markup {
              % Add color to markup in top staff
	      \override #'(baseline-skip . 0.5)
	      \column { \with-color #red \small { ^ 1 } }
	    }
	    s4.
	    \revert Beam.positions
	    \revert NoteHead.transparent
	    \revert NoteHead.duration-log
	  }
	\\
	  {
            % Add color to both Dashed Slurs in top staff
            \override Slur.color = #(x11-color "purple")
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    s1
	    \once \override Slur.height-limit = #6
	    \once \override Slur.extra-offset = #'(1.25 . 0)
	    \slurDashed
	    \I b2_( s2
	    \once \override NoteHead.transparent = ##t
	    b4) s
	    \once \override Slur.height-limit = #3.25
	    \once \override Slur.extra-offset = #'(.75 . 0)
	    a2_( s4
	    \once \override NoteHead.transparent = ##t
	    a4) g2
	    \revert Stem.transparent
	    \revert Flag.transparent
	  }
	\\
	  \override Staff.NoteCollision.merge-differently-headed = ##t
	  {
	    \override Beam.positions = #'(4 . -3.25)
	    \stemUp
	    g8[ s s4 s2
	    \stemDown
	    \once \override NoteHead.transparent = ##t
	    \I b8] s8
	    \override Beam.positions = #'(3 . -2.25)
	    \stemUp
	    a8[ s s4
	    \stemDown
	    c8] s s2 s s
	  }
	\\
	  {
            % Add color to all remaining Slurs in top staff
            \override Slur.color = #(x11-color "violet")
            \override PhrasingSlur.color = #(x11-color "violet")
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \override Stem.length = #0
            % Add color to text markups in top staff
	    g4_\( fis^(_\markup { \with-color #blue \tiny N } g)\)
	    a^(^\markup { \with-color #blue \tiny P } b2)
	    b4^(^\markup { \with-color #blue \tiny P }
	    \stemUp
	    \revert Stem.transparent
	    \revert Flag.transparent
	    \override Stem.length = #10
	    c8)^( s
	    \override Stem.length = #14
	    b4) s s
	    \override Stem.length = #0
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \once \override Slur.extra-offset = #'(0 . 0.35)
            % Add color to remaining text markup in top staff
	    c4^\( b_(_\markup { \with-color #blue \tiny P } a)\) s2
	    \revert Stem.length
	  }
	\\
	  {
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \override NoteHead.transparent = ##t
	    \override Stem.length = #0
	    s1 s4 e4 s
	    \change Staff = "LH"
	    fis,4 s2
	    \revert Stem.transparent
	    \revert Flag.transparent
	    \revert NoteHead.transparent
	    \revert Stem.length
	  }
	\\
	  {
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \override NoteHead.transparent = ##t
	    \override Stem.length = #0
	    s1 s s2
	    fis'4 s
	    \change Staff = "LH"
	    g,4 s s2
	    \revert Stem.transparent
	    \revert Flag.transparent
	    \revert NoteHead.transparent
	    \revert Stem.length
	  }
	>>
	\bar "|."
      }
    }

    \new Staff = "LH" { % Left hand
      \clef bass
      \key g \major
      \relative c' {
	\override Staff.NoteCollision.merge-differently-headed = ##t
	<<
	  {
	    \override Beam.positions = #'(-8 . -8)
	    \override NoteHead.transparent = ##t
	    \stemDown
            % Add color to long beam text markups in bottom staff
	    \I g8[_\markup { \with-color #(x11-color 'LawnGreen) \bold I }
            s4. s1 s s2
	    \I d8_\markup { \with-color #(x11-color 'LawnGreen) \bold V }
            s4.
	    \I g,8]_\markup { \with-color #(x11-color 'LawnGreen) \bold I }
            s4.
	    \revert Beam.positions
	    \revert NoteHead.transparent
	  }
	\\
	  {
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \stemDown
	    \override TextScript.extra-offset = #'(-11.75 . -12.25)
	    \I g'2 s1 s s2 \I d2 g,2
	    \revert Stem.transparent
	    \revert Flag.transparent
	  }
	\\
	  {
            % Add color to all single-note Slurs in bottom staff
            \override Slur.color = #(x11-color "violet")
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \once \override NoteHead.transparent = ##t
	    \override Stem.length = #0
	    g'4
	    \once \override TextScript.padding = #0.25
            % Add color to text markups in bottom staff
	    a4_(^\markup { \with-color #blue \tiny P } b)
	    fis4^(^\markup { \with-color #blue \tiny P } e)
	    \once \override NoteHead.transparent = ##t
	    \once \override Slur.height-limit = #1.5
            % Add color to remaining text markup in bottom staff
	    c4^( d)^\markup { \with-color #blue \tiny N }
	    \once \override NoteHead.transparent = ##t
	    \once \override Slur.extra-offset = #'(0 . 0.5)
	    \I fis,4_(
	    \revert Stem.transparent
	    \revert Flag.transparent
	    \override Stem.length = #10
	    \stemDown
	    g4) s
	    \once \override Slur.extra-offset = #'(0 . 0.25)
	    \I c8_( s
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \revert Stem.length
	    a4)
	    \once \override NoteHead.transparent = ##t
	    \I d4^( d,4) s2
	  }
	\\
	  {
            % Add color to all two-note Slurs in bottom staff
            \override Slur.color = #(x11-color "violet")
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \override NoteHead.transparent = ##t
	    \I g'4^( s b) s2
	    \revert Stem.transparent
	    \revert Flag.transparent
	    \revert NoteHead.transparent
	    \override Beam.positions = #'(-4 . 1)
	    \stemDown
	    c,8[ s s4
	    \stemUp
	    fis,8] s
	    \override Beam.positions = #'(1 . -4)
	    g8[ s
	    \stemDown
	    b8] s
	    \revert Beam.positions
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \override NoteHead.transparent = ##t
	    c4^( s d4) s s2
	  }
	\\
	  {
            % Add color to four-note Slur in bottom staff
            \override Slur.color = #(x11-color "violet")
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \override NoteHead.transparent = ##t
	    \override Stem.length = #0
	    \stemDown
	    \once \override Slur.height-limit = #3
	    \once \override Slur.extra-offset = #'(0 . 0.25)
	    \I g4_( s2. e4) s2. s2 s1 s2
	    \revert Stem.transparent
	    \revert Flag.transparent
	    \revert NoteHead.transparent
	  }
	\\
	  {
            % Add color to dashed Slur in bottom staff
            \override Slur.color = #(x11-color "purple")
	    \override Stem.transparent = ##t
	    \override Flag.transparent = ##t
	    \override NoteHead.transparent = ##t
	    \slurDashed
	    \once \override Slur.height-limit = #6.0
	    \once \override Slur.extra-offset = #'(0.5 . -0.25)
	    \override Stem.length = #0
	    g4_( s2. s1 g,4) s s1 s2
	    \revert Stem.transparent
	    \revert Flag.transparent
	    \revert NoteHead.transparent
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
  \layout {
    indent = 0.0
    ragged-right = ##f
    \context { \Staff \remove "Time_signature_engraver" }
  }
}
