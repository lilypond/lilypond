\version "2.11.61"

% this chart is used in the manual too.

\header {
    texidoc ="@cindex Feta scripts

This chart shows all articulations, or scripts, that feta font contains.

"
}

\score {
    <<
      \override Score.LyricText  #'font-family = #'typewriter
      \override Score.LyricText  #'font-shape = #'upright
      \context Staff  {
	\set Score.timing = ##f
	\set Score.barAlways = ##t
        \override Score.SeparationItem #'padding = #2.5
	\override Staff.BarLine  #'transparent = ##t
        c''\accent             c''\marcato            c''\staccatissimo 	c''\espressivo
        c''\staccato           c''\tenuto             c''\portato
        c''\upbow              c''\downbow            c''\flageolet
        c''\thumb              c''^\lheel             c''\rheel
        c''^\ltoe              c''\rtoe               c''\open
        c''\stopped            c''\turn               c''\reverseturn
        c''\trill              c''\prall              c''\mordent
        c''\prallprall         c''\prallmordent       c''\upprall
        c''\downprall          c''\upmordent          c''\downmordent
        c''\pralldown          c''\prallup            c''\lineprall
        c''\signumcongruentiae c''\shortfermata       c''\fermata
        c''\longfermata        c''\verylongfermata    c''\segno
        c''\coda               c''\varcoda
      }
      \context Lyrics \lyricmode {
        accent__                marcato__               staccatissimo__		espressivo__
        staccato__              tenuto__                portato__
        upbow__                 downbow__               flageolet__
        thumb__                 lheel__                 rheel__
        ltoe__                  rtoe__                  open__
        stopped__               turn__                  reverseturn__
        trill__                 prall__                 mordent__
        prallprall__            prallmordent__          upprall__
        downprall__             upmordent__             downmordent__
        pralldown__             prallup__               lineprall__
        signumcongruentiae__    shortfermata__          fermata__
        longfermata__           verylongfermata__       segno__
        coda__                  varcoda__
      }
    >>
    \layout {
      line-width = 5.1\in
      indent = 0.0\mm
    }
  }


