
\version "2.8.0"
% check with invisible-notes or blank-notes.  possible rename. -gp
\header{ texidoc = "@cindex Partial Blank
When entering partially typeset music (i.e. for students to be 
completed by hand), you may need the spacing that correspond to the 
timing of notes: all measures have same length, etc.  It can be
implemented by adding an invisible staff with a lot of fast notes. "
}

quickmeasure =  {
    \repeat unfold 16 c''16
}

mel =  \relative c' {c16 d16 e8 a4 g2 e8 d c2. g'1 e4 d c2}

\score {
\context PianoStaff  <<
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
  \new Staff  {\clef F s1*4}
>>
\layout {}
}

