\version "1.3.120";

\include "paper16.ly";

% #(set! point-and-click #t)
#(define text-flat '((font-relative-size . -2) (music "accidentals--1")))


% Coriolan 218-222
flautoI = \notes\relative c'' {
  \property Score.currentBarNumber = #218
  des2.()c4|e(f e)f|\break
  r2 des4\sf()c|r2 des4\sf()c|
}
flautoII = \notes\relative c'' {
 g2.()as4|bes(as bes)as|
 R1*2
}
oboeI = \notes\relative c'' {
 e2.()f4|e(f e)f|
 r2 as|r as\sf|
}
oboeII = \notes\relative c'' {
 g2.()as4|bes(as bes)as|
 r2 as'|r as\sf|
}
clarinettoI = \notes\relative c' {
  es2.()d4|c(bis c)bes|
% r2 es4\sf()d|r2 es4\sf()d|
}
clarinettoII = \notes\relative c' {
  es2.()d4|c(bes c)bes|
% r2 es'4\sf()d|r2 es4\sf()d|
}
fagottoI = \notes\relative c' {
  bes2.()as4|g(f g)f|
  r r8 des' des4\sf()es|r r8 des des4\sf()es|
}
fagottoII = \notes\relative c' {
  bes2.()as4 | g(f g)f|
  f4 r r2 | f4 r r2|
}
cornoI = \notes\relative c''' {
  g2. f4|g f g f|
% r4 r8 f f2|r4 r8 f f2|
}
cornoII = \notes\relative c''' {
  g,2. d'4|g, d' g, d'|
  r4 r8 d d4\sf()es|r4 r8 d d4\sf()es|
}
tromboI = \notes\relative c'' {
  c2. c4|c c c c|
  R1*2|
}
tromboII = \notes\relative c' {
  c2. c4|c c c c|
  R1*2|
}
timpani = \notes\relative c {
  c2:16 c4 c|c c c c |
  R1*2|
}
violinoI = \notes\relative c' {
 des'2:16\ff des4: c:|e,: f: e: f:|
 r8 as des,4\sf~des()c |r8 as' des,4\sf~des()c|
}
violinoII = \notes\relative c' {
  des2:16\ff des4: c: |  bes: as: bes: as:|
% r8 as des,4\sf ~ des()c| r8 as' des,4\sf ~ des()c|
}
violaI = \notes\relative c' {
  e2:16\ff e4: f:|b,: c: b: c:|
  r4 r8 as as2\sf|r4 r8 as as2\sf|
}
violaII = \notes\relative c' {
  bes2:16\ff bes4: as:|g: f: g: f: |
  r4 r8 f f4\sf()ges|r4 r8 f f4\sf()ges|
}
violoncello = \notes\relative c {
  bes2.\ff()as4|g( f g )f |
  r4 r8 f' f4\sf()ges |r4 r8 f f4\sf()ges |
}
contrabasso = \notes\relative c {
  bes2.\ff()as4|g( f g )f|
  f4 r r2 |f'4 r r2|
}



%%
%% Hmm, can't we move this to a `template.ly'
%%
\score {
  < 
    \context StaffGroup = wood <
      \context Staff = flauti <
	\property Staff.midiInstrument = #"flute"
	\property Staff.instrument = "2 Flauti"
	\property Staff.instr = "Fl."
	\context Voice=one \partcombine Voice
	  \context Thread=one \flautoI
	  \context Thread=two \flautoII
      >
      \context Staff = oboes <
      	\property Staff.midiInstrument = #"oboe"
	\property Staff.instrument = "2 Oboi"
	\property Staff.instr = "Ob."
	\context Voice=one \partcombine Voice
	  \context Thread=one \oboeI
	  \context Thread=two \oboeII
      >
      \context Staff = clarinets <
        \property Staff.midiInstrument = #"clarinet"
	\property Staff.instrument = #`((kern . 0.5)
	  (lines "2 Clarinetti" (rows "(B" ,text-flat ")")))
	\property Staff.instr = #`((kern . 0.5)
	  (lines "Cl."  (rows "(B" ,text-flat ")")))
	\property Staff.transposing = #-2
	\notes \key f \major;
	\context Voice=one \partcombine Voice
	  \context Thread=one \clarinettoI
	  \context Thread=two \clarinettoII
      >
      \context Staff = bassoons <
	\property Staff.midiInstrument = #"bassoon"
	\property Staff.instrument = "2 Fagotti"
	\property Staff.instr = "Fg."
	\clef bass;
	\context Voice=one \partcombine Voice
	  \context Thread=one \fagottoI
	  \context Thread=two \fagottoII
      >
    >
    \context StaffGroup = brass <
      \context Staff = frenshHorns <
        \property Staff.midiInstrument = #"french horn"
	\property Staff.instrument = #`((kern . 0.5)
	  (lines "2 Corni" (rows "(E" ,text-flat ")")))
	\property Staff.instr = #`((kern . 0.5)
	  (lines "Cor."  (rows "(E" ,text-flat ")")))
	\property Staff.transposing = #3
	\notes \key c \major;
	\context Voice=one \partcombine Voice
	  \context Thread=one \cornoI
	  \context Thread=two \cornoII
      >
      \context Staff = trumpets <
  	\property Staff.midiInstrument = #"clarinet"
	\property Staff.instrument = #`((kern . 0.5)
	  (lines "2 Trombe" (rows "(C)")))
	\property Staff.instr = #`((kern . 0.5)
	  (lines "Tbe." (rows "(C)")))
	\context Voice=one \partcombine Voice
	  \context Thread=one \tromboI
	  \context Thread=two \tromboII
      >
    >
    \context StaffGroup = timpani <
      \context Staff = timpani <
	\property Staff.midiInstrument = #"timpani"
	\property Staff.instrument = #'((kern . 0.5)
	  (lines "2 Timpani" "(C-G)"))
	\property Staff.instr = #"Timp."
	\clef bass;
	\timpani
      >
    >
    \context StaffGroup = strings <
      \context GrandStaff = violins <
	\context Staff = viI <
	  \property Staff.midiInstrument = #"violin"
	  \property Staff.instrument = "Violino I"
	  \property Staff.instr = "Vi. I"
	  \violinoI	  
	>
	\context Staff = viII <
	  \property Staff.midiInstrument = #"violin"
	  \property Staff.instrument = "Violino II"
	  \property Staff.instr = "Vi. II"
	  \violinoII
	>
      >
      \context Staff = vla <
      	\property Staff.midiInstrument = #"viola"
 	\property Staff.instrument = "Viola"
	\property Staff.instr = "Vla."
	\clef alto;
	\context Voice=one \partcombine Voice
	  \context Thread=one \violaI
	  \context Thread=two \violaII
      >
      \context GrandStaff=bass <
        \property GrandStaff.soloADue = ##t
        \property GrandStaff.soloText = #""
        \property GrandStaff.soloIIText = #""
	% This is non-conventional, but currently it is
	% the only way to tell the difference.
        \property GrandStaff.aDueText = #"\\`a2"
        \property GrandStaff.splitInterval = #'(1 . 0)
        \property GrandStaff.changeMoment =
	  #`(,(make-moment 1 1) . ,(make-moment 1 1))

        \context Staff=one <
	  \property Staff.midiInstrument = #"cello"
	  \property Staff.instrument = #'((kern . 0.5)
	    (lines "Violoncello" (rows "    e") (rows "Contrabasso")))
	  \property Staff.instr = "Vc."
	  \clef bass;
	>
	\context Staff=two <
	  \property Staff.midiInstrument = #"contrabass"
	  \property Staff.instrument = "Contrabasso"
	  \property Staff.instr = "C.B."
	  \clef bass;
	  \skip 1*4; % sustain clef
	>
	\context Staff=one \partcombine Staff
	  \context Voice=one \violoncello
	  \context Voice=two \contrabasso
      >
    >
  >
  \paper {
    \paperSixteen
    linewidth = 80 * \staffspace;
    textheight = 200 * \staffspace;
    \translator{
      \ThreadContext
      \consists "Rest_engraver";
    }
    \translator{
      \VoiceContext
      \remove "Rest_engraver";    
      soloText = #"I."
      soloIIText = #"II."
      soloADue = ##f
      % We must override the settings for Staff level
      % with the default values, here.
      aDueText = #"\\`a2"
      splitInterval = #'(0 . 1)
      changeMoment = #`(,(make-moment 0 0) . ,(make-moment 1 512))
    }
    \translator{
      \HaraKiriStaffContext
      \consists "Mark_engraver";
      soloADue = ##t
      soloText = #""
      soloIIText = #""
      % This is non-conventional, but currently it is
      % the only way to tell the difference.
      aDueText = #"\\`a2"
      splitInterval = #'(1 . 0)
      changeMoment = #`(,(make-moment 1 1) . ,(make-moment 1 1))
    }
    \translator {
      \OrchestralScoreContext
      skipBars = ##t
      % Hmm
      currentBarNumber = #218
      BarNumber \override #'padding = #3
      RestCollision \override #'maximum-rest-count = #1
      marginScriptHorizontalAlignment = #1
      TimeSignature \override #'style = #'C
    }
    \translator { \HaraKiriStaffContext }
  }
}

