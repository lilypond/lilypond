
\version "2.1.22"
% check with invisible-notes or blank-notes.  possible rename. -gp
\header{ texidoc = "@cindex Partial Blank
When entering half music (i.e. for students to complete by hand)
you need the spacing to correspond to the timing -- all measures
same length, etc.  This thing implements it by adding invisible
staff with lots of fast notes. "
}

quickmeasure = \notes {
    \repeat unfold 16 c''16
}

mel = \notes \relative c' {c16 d16 e8 a4 g2 e8 d c2. g'1 e4 d c2}

\score {
\context PianoStaff \notes <<
  \new Staff <<
    \clef G
    \new Voice {\mel}
    \new Voice {
	\override NoteHead  #'transparent = ##t
	\override Stem  #'transparent = ##t
	\override Beam  #'transparent = ##t
        \repeat unfold 4 \quickmeasure
    }
  >>
  \new Staff \notes {\clef F s1*4}
>>
\paper {}
}

