
\version "2.8.0"


% Ugh, we need to override some LaTeX titling stuff
\header {
  title =	"Ouvertüre"
  subtitle =	"Zu Heinrich Joseph v. Collins Trauerspiel"
  subsubtitle =	\markup { \large \bold "Coriolan" }
  opus = "Op. 62"
  piece = \markup { \large Allegro con brio }
  composer = 	 "Ludwig van Beethoven (1770-1827)"

texidoc = "@cindex Orchestra Score
@cindex Coriolan Score
In an orchestral score (Beethoven's Coriolan overture), there are 
different instrument groups, and some of the instruments may be
transposed. Instruments are indicated either with a long or short name.
" }

#(set-global-staff-size 16)

raisedFlat = \markup { \raise #0.4 \smaller \smaller \flat  }



flauti =  \relative c' {
  \set Staff.instrument = #"2 Flauti"
  \set Staff.instr = #"Fl."
  \time 4/4
  c1 
  \break c
%  \break c
%  \break c
  \bar"|."
}

oboi =  \relative c' {
  \set Staff.instrument = #"2 Oboi"
  \set Staff.instr = #"Ob."
  c1 c
}

clarinetti =  \relative c' {
    \set Staff.instrument = \markup { \column { "Clarinetti" \line { "in B" \raisedFlat } } }
    \set Staff.instr = \markup { \smaller  { "Cl(B" \raisedFlat ")" } }

  c1 c
}

fagotti =  \relative c' {
  \set Staff.instrument = #"2 Fagotti"
  \set Staff.instr = #"Fg."
  c1 c
}

corni =  \relative c' {
    \set Staff.instrument = \markup { \column { "Corni" \line { "in E" \raisedFlat } } }
    \set Staff.instr = \markup { \smaller  { "Cor(E" \raisedFlat ")" } }

  c1 c
}

trombe =  \relative c' {
  \set Staff.instrument = \markup \column { "2 Trombe" "(C)" }
  \set Staff.instr = \markup \column {  "Tbe." "(C)" }

  c1 c
}

timpani =  \relative c' {
  \set Staff.instrument = \markup \column { "Timpani" "(C-G)" }
  \set Staff.instr = #"Timp."

  c1 c
}

violinoI =  \relative c' {
  \set Staff.instrument = #"Violino I  "
  \set Staff.instr = #"Vl. I  "
  c1 c
}

violinoII =  \relative c' {
  \set Staff.instrument = #"Violino II  "
  \set Staff.instr = #"Vl. II  "
  c1 c
}

viola =  \relative c' {
  \set Staff.instrument = #"Viola"
  \set Staff.instr = #"Vla."
  c1 c 
  %c
}

violoncello =  \relative c' {
  \set Staff.instrument = \markup \column { "Violoncello" "e" "Contrabasso" }
  \set Staff.instr = \markup \column {  "Vc." "Cb." }
  c1 c
}


\paper {
    indent=10.0\mm
    line-width=150.0\mm
    ragged-bottom = ##t 
}

#(set-global-staff-size 16)
\book {
    \score {
      << 
	\new StaffGroup = "legni" << 
	  \new Staff = "flauti" \flauti
	  \new Staff = "oboi" \oboi
	  \new Staff = "clarinetti" \clarinetti 
	  \new Staff = "fagotti" \fagotti 
	>>
	\new StaffGroup = "ottoni" <<
	  \new Staff = "corni" \corni
	  \new Staff = "trombe" \trombe
	>>
	\new StaffGroup = "timpani" <<
	  \new Staff = "timpani" \timpani
	 { 
	   \skip 1 
	   % Hmm: this forces a staff-bracket, that's good!
	   % However, I can't find where is decided on staff-bracket yes/no
	 }
	>>
	\new StaffGroup = "archi" <<
	  \new GrandStaff = "violini" <<
	    \new Staff = "violinoI" \violinoI
	    \new Staff = "violinoII" \violinoII
	  >>
	  \new Staff = "viola" \viola
	  \new Staff = "violoncello" \violoncello
	>>
      >>
      \layout {
	  \context {
	      \RemoveEmptyStaffContext
	  }
	  \context {
	      \Score
	      \override TimeSignature #'style = #'C
	  }
      }
    }
    % this is ignored?
    \paper {
	indent=10.0\mm
	line-width=150.0\mm
    }
}

%% Local Variables:
%% coding: utf-8
%% End:
