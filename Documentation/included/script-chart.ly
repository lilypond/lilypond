\version "2.12.0"

\header {
  texidoc ="@cindex Feta scripts
This chart shows all articulations, or scripts, that the feta font contains.
"
}

\score {
  <<
    \new Voice = "scripts" {
      c''\accent             c''\marcato         c''\staccatissimo c''\espressivo
      c''\staccato           c''\tenuto          c''\portato
      c''\upbow              c''\downbow         c''\flageolet
      c''\thumb              c''^\lheel          c''\rheel
      c''^\ltoe              c''\rtoe            c''\open          c''\halfopen
      c''\stopped            c''\snappizzicato   c''\turn          c''\reverseturn
      c''\trill              c''\prall           c''\mordent
      c''\prallprall         c''\prallmordent    c''\upprall
      c''\downprall          c''\upmordent       c''\downmordent
      c''\pralldown          c''\prallup         c''\lineprall
      c''\signumcongruentiae c''\shortfermata    c''\fermata
      c''\longfermata        c''\verylongfermata c''\segno
      c''\coda               c''\varcoda
    }
    \new Lyrics \lyricsto "scripts" {
      accent             marcato         staccatissimo espressivo
      staccato           tenuto          portato
      upbow              downbow         flageolet
      thumb              lheel           rheel
      ltoe               rtoe            open          halfopen
      stopped            snappizzicato   turn          reverseturn
      trill              prall           mordent
      prallprall         prallmordent    upprall
      downprall          upmordent       downmordent
      pralldown          prallup         lineprall
      signumcongruentiae shortfermata    fermata
      longfermata        verylongfermata segno
      coda               varcoda
    }
  >>
  \layout {
    line-width = 5.1\in
    indent = 0.0\mm
    \context {
      \Score
      timing = ##f
      barAlways = ##t
      \override NonMusicalPaperColumn #'padding = #2.5
    }
    \context {
      \Staff
      \remove "Time_signature_engraver"
      \override BarLine #'transparent = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'font-family = #'typewriter
      \override LyricText #'font-shape = #'upright
    }
  }
}
