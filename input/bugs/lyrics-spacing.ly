\include "norsk.ly"

\header {
texidoc ="``baz'' should be centered on the a (tenor voice), but it is
not."
}

\version "1.3.148"

sop = \notes \transpose c''  {e2 e fis1 }
alt = \notes \relative c' {cis2 e e d }
ten = \notes \relative c' {a2 a a1 }
txt = \lyrics {foo2 bar baz jazz }

\score {
    <
	\context Staff = up \notes <
	    \context Voice=sopv {\stemUp \sop}
 	    \context Voice=altv {\stemDown \alt}
	>
	\context Staff = down \notes < \clef "F"
	    \context Voice=tenv {\stemUp \ten}
	>
	\context Lyrics = la { \txt }
    >
    \paper { linewidth = -1 }
}
