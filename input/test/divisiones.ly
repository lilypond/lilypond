% possible rename to ancient- or gregorian- ?
\header {
    texidoc = "@cindex Divisiones

Divisiones are gregorian variants of breathing signs.
Choices are @code{divisioMinima}, @code{divisioMaior},
@code{divisioMaxima} and @code{finalis}, @code{virgula} and
@code{caesura}.

" }

\version "2.3.22"

\include "gregorian-init.ly"

\score {
 <<
	\context Voice  \transpose c c' {
	    \set Score.timing = ##f
	    \set Score.barAlways = ##t
	    \override TextScript  #'padding = #3
	    \override Staff.BarLine  #'transparent = ##t
	    \override Stem  #'transparent = ##t
            g4( a) g^\markup { "divisio minima" }
	    \divisioMinima
            g4( a) g^\markup { "divisio maior" }
	    \divisioMaior
            g4( a) g^\markup { "divisio maxima" }
	    \divisioMaxima
            g4( a) g^\markup { "  finalis" }
	    \finalis
            g4( a) g
            ^\markup { "virgula" }
	    \virgula
            g4( a) g
            ^\markup { "caesura" }
	    \caesura
            g4( a) g
	}
	\lyricsto "" \new  Lyrics \lyricmode {
	    Blah blub, blah blam.
	    Blah blub, blah blam.
	    Blah blub, blah blam.
	    Blah blub.
	}
    >>
}
