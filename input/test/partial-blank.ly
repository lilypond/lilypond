#(ly:set-option 'old-relative)
\version "1.9.0"
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
\context PianoStaff \notes <
  \context Staff = v <
    \clef G
    \context Voice=melo {\mel}
    \context Voice=invisible {
	\property Voice.NoteHead \override #'transparent = ##t
	\property Voice.Stem \override #'transparent = ##t
	\property Voice.Beam \override #'transparent = ##t
        \repeat unfold 4 \quickmeasure
    }
  >
  \context Staff = empty \notes {\clef F s1*4}
>
\paper {}
}

