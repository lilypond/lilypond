\version "1.3.146"


% Ugh, we need to override some LaTeX titling stuff
\header {
  title =	"Ouvertüre\\vrule width0pt height 0pt depth 1ex"
  subtitle =	"\\normalsize Zu Heinrich Joseph v. Collins Trauerspiel\\vrule width0pt height 0pt depth 1ex"
  subsubtitle =	"\\Large\\bf Coriolan"
  opus =  "Op. 62"
  piece = "\\hspace*{30mm}\\normalfont\\large Allegro con brio"
  composer = 	 "Ludwig van Beethoven (1770-1827)"
  enteredby = 	 "JCN"
}
#(define raisedflat '((raise . 0.4) (music (named "accidentals--1"))))

flauti = \notes \relative c' {
  \property Staff.instrument	= #"2 Flauti"
  \property Staff.instr		= #"Fl."
  \time 4/4
  c1 
  \break c
%  \break c
%  \break c
  \bar"|."
}

oboi = \notes \relative c' {
  \property Staff.instrument	= #"2 Oboi"
  \property Staff.instr		= #"Ob."
  c1 c
}

clarinetti = \notes \relative c' {
%   \property Staff.instrument = #`("Clarinetti in B" ,text-flat)
  \property Staff.instrument	= #`(lines "2 Clarinetti" (columns "(B" ,raisedflat ")"))
  \property Staff.instr		= #`(lines "Cl." (columns "(B" ,raisedflat ")"))

  c1 c
}

fagotti = \notes \relative c' {
  \property Staff.instrument	= #"2 Fagotti"
  \property Staff.instr		= #"Fg."
  c1 c
}

corni = \notes \relative c' {
  \property Staff.instrument	= #`(lines "2 Corni" (columns "(E" ,raisedflat ")"))
  \property Staff.instr		= #`(lines "Cor." (columns "(E" ,raisedflat ")"))

  c1 c
}

trombe = \notes \relative c' {
  \property Staff.instrument	= #'(lines "2 Trombe" "(C)")
  \property Staff.instr		= #'(lines "Tbe." "(C)")

  c1 c
}

timpani = \notes \relative c' {
  \property Staff.instrument	= #'(lines "Timpani" "(C-G)")
  \property Staff.instr		= #"Timp."

  c1 c
}

violinoI = \notes \relative c' {
  \property Staff.instrument	= #"Violino I  "
  \property Staff.instr		= #"Vl. I  "
  c1 c
}

violinoII = \notes \relative c' {
  \property Staff.instrument	= #"Violino II  "
  \property Staff.instr		= #"Vl. II  "
  c1 c
}

viola = \notes \relative c' {
  \property Staff.instrument	= #"Viola"
  \property Staff.instr		= #"Vla."
  c1 c 
  %c
}

violoncello = \notes \relative c' {
  \property Staff.instrument	= #'(lines "Violoncello" "e" "Contrabasso")
  \property Staff.instr		= #'(lines "Vc." "Cb.")
  c1 c
}

\include "paper16.ly"

\score {
  < 
    \context StaffGroup ="legni" < 
      \context Staff ="flauti" \flauti
      \context Staff ="oboi" \oboi
      \context Staff ="clarinetti" \clarinetti 
      \context Staff ="fagotti" \fagotti 
    >
    \context StaffGroup ="ottoni" <
      \context Staff ="corni" \corni
      \context Staff ="trombe" \trombe
    >
    \context StaffGroup ="timpani" <
      \context Staff ="timpani" \timpani
     { 
       \skip 1 
       % Hmm: this forces a staff-bracket, that's good!
       % However, I can't find where is decided on staff-bracket yes/no
     }
    >
    \context StaffGroup ="archi" <
      \context GrandStaff ="violini" <
        \context Staff ="violinoI" \violinoI
        \context Staff ="violinoII" \violinoII
      >
      \context Staff ="viola" \viola
      \context Staff ="violoncello" \violoncello
    >
  >
  \paper {
  	\paperSixteen
  	indent=100.0\mm
  	linewidth=150.0\mm
    \translator {
      \HaraKiriStaffContext
    }
    \translator {
      \OrchestralScoreContext
      TimeSignature \override #'style = #'C
    }
  }
}

