\version "1.3.146"
\header{
  latexpackages="graphicx"
}



m =  \notes \relative c''{

c1 | c2 c | c c | c c | \break c c | c c | c c | c c | 
}

M =  \notes \relative c''{

c1 | c2 c | c c | R1*5 
}

\score{ < 
  \context StaffGroup = wood <
    \context Staff = flauto <
    %"\\rotatebox[origin=c]{90}{Flauto}"
    
      \property Staff.instrument = "Flauto"
      \property Staff.instr = "Fl."
      \m
    >
    \context Staff = oboe <
      \property Staff.instrument = "Oboe"
      \property Staff.instr = "Ob."
      \m
    >
    \context Staff = clarI <
      \property Staff.instrument = "Clarinetto I"
      \property Staff.instr = "Cl. I"
      \m
    >
    \context Staff = clarII <
      \property Staff.instrument = "Clarinetto II"
      \property Staff.instr = "Cl. II"
      \m
    >
    \context Staff = fagotto <
      \property Staff.instrument = "Fagotto"
      \property Staff.instr = "Fg."
      \m
    >
  >
  \context StaffGroup = brass <
    \context Staff = cor <
      \property Staff.instrument = "2 Corni in F"
      \property Staff.instr = "Cor."
      \context Voice = corI { \stemUp \M }
      \context Voice = corII { \stemDown \M }
    >
    \context Staff = trp <
      \property Staff.instrument = #`(columns "2 Trp. in B " (music "accidentals--1"))
      \property Staff.instr = "Trp."
      \context Voice = trpI { \stemUp \M }
      \context Voice = trpII { \stemDown \M }
    >
  >
    \context StaffGroup = percussion <\context Staff = timpani <
      \property Staff.instrument = "Timpani"
      \property Staff.instr = "Timp."
      \notes{c''1 R1*8}
    >
  >
  \context StaffGroup = strings <
    \context GrandStaff = violins <
      \context Staff = viI <
        \property Staff.instrument = "Violin I"
        \property Staff.instr = "Vi. I"
        \m
      >
      \context Staff = viII <
        \property Staff.instrument = "Violin II"
        \property Staff.instr = "Vi. II"
        \m
      >
    >
    \context Staff = vla <
      \property Staff.instrument = "Viola"
      \property Staff.instr = "Vla."
      \m
    >
    \context Staff = vlc <
      %% \property Staff.instrument = "Violoncello"
      \property Staff.instrument = #'(lines "Violoncello" "e" "Contrabasso")
      \property Staff.instr = "Vlc"
      \m
    >
    \context Staff = cb <
      \property Staff.instrument = "Contrabasso"
      \property Staff.instr = "C.B."
      \m
    >
  >
>
 \paper {
%    \paperSixteen
    linewidth = 185.\mm
    textheight = 260.\mm
    \translator {
      \OrchestralScoreContext
      skipBars = ##t 
      markScriptPadding = #4.0
      BarNumber \override #'padding = #3
      RestCollision \override #'maximum-rest-count = #1
      marginScriptHorizontalAlignment = #1
    }
    \translator { \HaraKiriStaffContext
    }
  }
}

