% Bugs:
% * CLef has `Staff' hardcoded
% * Dynamics into staff: pp/ff at first note cello/contrabass

\header {
texidoc="Template for part-combining orchestral scores";
}
  
\include "paper16.ly"; 
% \include "mutopia/Coriolan/coriolan-paper.ly";



flautoI = \notes\relative c'' {
  c4 d e f
  b,4 d c d
  r2 e4 f
  \break
  c4 d e f
  c4 r e f
  c4 r e f
  \break
  c4 r a r
  a a r a
  a2 \property VoiceCombineThread.soloADue = ##f a
}

flautoII = \notes\relative c'' {
  g4 b d f
  r2 c4 d
  a c c d
  a4. b8 c4 d
  c r e r
  r2 s2
  a,4 r a r
  a r r a
  a2 \property VoiceCombineThread.soloADue = ##f a
}
          
flautiStaff =  \notes \context VoiceCombineStaff = flauti <
  \property VoiceCombineStaff.midiInstrument = #"flute"
  \property VoiceCombineStaff.instrument = #"2 Flauti"
  \property VoiceCombineStaff.instr = #"Fl."
  %\global
  \context VoiceCombineVoice=one \partcombine VoiceCombineVoice
    \context VoiceCombineThread=one \flautoI
    \context VoiceCombineThread=two \flautoII
>

legniGroup =  \context StaffGroup = legni_group <
  \flautiStaff
  %\oboiStaff
  %\clarinettiStaff
  %\fagottiStaff
>

violinoI = \notes\relative c'' {
  c4 d e f
  c d e f
  c d e f
  c d e f
  c d e f
  c d e f
  c4 d e f
  a8 a a a b b b b
  d1
}      

violinoII = \notes\relative c'' { 
  c4 d e f
  c d e f
  c d e f
  c2 e2
  c4 d e f
  c2 e2
  c,4 d e f
  a8 a a a b b b b
  b1
}

violinoIStaff =  \context Staff = oneViolino <
 \property Staff.midiInstrument = #"violin"
  \property Staff.instrument = #"Violino I"
  \property Staff.instr = #"Vl. I"
  %\notes < 
    %\global
    %\context Voice=oneViolino
    %\context Thread=oneViolino
    \violinoI
 % >
>

violinoIIStaff =  \context Staff = twoViolino <
  % MIDI hoort geeneens verschil tussen een
  % eerste en tweede viool ;-)
  \property Staff.midiInstrument = #"violin"
  \property Staff.instrument = #"Violino II"
  \property Staff.instr = #"Vl. II"
  %\notes < 
    %\global
    %\context Voice=twoViolino
    %\context Thread=twoViolino
    \violinoII
 % >
>

violaI = \notes\transpose c, \violinoI

violaII = \notes\transpose c, \violinoII

violiGroup =  \notes \context VoiceCombineStaff = oneVioli <
  \property VoiceCombineStaff.midiInstrument = #"viola"
  \property VoiceCombineStaff.instrument = #"Viola"
  \property VoiceCombineStaff.instr = #"Vla."
  \clef "alto";

  \key f \major;
  \skip 1*314; 
  \bar "|.";

  \context VoiceCombineVoice=oneVioli \partcombine VoiceCombineVoice
    \context VoiceCombineThread=oneVioli \violaI
    \context VoiceCombineThread=twoVioli \violaII
>

violoncello = \notes\relative c {
  c1\ff d e f c d e f c
}

contrabasso = \notes\relative c {
  c1\pp
  d4 e d e
  e1
  f4 g f g
  c1
  d4 e d e
  e1
  f4 g f g
  c1
}


bassiGroup =  \context PianoStaff = bassi_group \notes <
  \context StaffCombineStaff=oneBassi {
    \property StaffCombineStaff.midiInstrument = #"cello"
    %\property StaffCombineStaff.instrument = #"Violoncello\ne\nContrabasso"
    \property StaffCombineStaff.instrument = #'(lines "Violoncello" "e" "Contrabasso")
    \property StaffCombineStaff.instr = #"Vc."
    
    \clef "bass";
    \key es \major;
    \skip 1*314; 
    \bar "|.";
  }
  \context StaffCombineStaff=twoBassi {
    \property StaffCombineStaff.midiInstrument = #"contrabass"
    \property StaffCombineStaff.instrument = #"Contrabasso"
    \property StaffCombineStaff.instr = #"Cb."
    
    \clef "bass";
    \key as \major;
    \skip 1*314; 
    \bar "|.";
  }

  \context StaffCombineStaff=oneBassi \partcombine StaffCombineStaff
    \context StaffCombineVoice=oneBassi \violoncello
    \context StaffCombineVoice=twoBassi \contrabasso
>


violiniGroup =  \context GrandStaff = violini_group <
  \violinoIStaff
  \violinoIIStaff
>

archiGroup =  \context StaffGroup = archi_group <
  \violiniGroup
  \violiGroup
  \bassiGroup
>


\score{
  <
    \legniGroup
    %\ottoniGroup
    %\timpaniGroup
    \archiGroup
  >
  \header {
    title = "Coriolan";
    subtitle = "Ouverture"; 
    opus = "Opus 62";
    composer = "Ludwig van Beethoven (1770-1827)";
    enteredby = "JCN";
    copyright = "public domain";
  }
  %\paper {}
  \paper{
    \paperSixteen

    %textheight = 290.0\mm;
    %linewidth = 195.0\mm;
    textheight = 285.0\mm;
    linewidth = 190.0\mm;

    \translator{ \HaraKiriStaffContext }
    %
    % The Voice combine hierarchy
    %
    \translator{
      \ThreadContext
      \name "VoiceCombineThread";
      \consists "Rest_engraver";
    }
    \translator{
      \VoiceContext
      \name "VoiceCombineVoice";
      soloText = #"I."
      soloIIText = #"II."
      \remove "Rest_engraver";
      \accepts "VoiceCombineThread";
    }
    \translator{
      \HaraKiriStaffContext
      \consists "Mark_engraver";
      \name "VoiceCombineStaff";
      \accepts "VoiceCombineVoice";
    }

    %
    % The Staff combine hierarchy
    %
    \translator{
      \ThreadContext
      \name "StaffCombineThread";
    }
    \translator{
      \VoiceContext
      \name "StaffCombineVoice";
      \accepts "StaffCombineThread";
      \consists "Thread_devnull_engraver";
    }
    \translator {
      \HaraKiriStaffContext
      \name "StaffCombineStaff";
      \accepts "StaffCombineVoice";

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
      \StaffGroupContext
      \accepts "VoiceCombineStaff";
      \accepts "StaffCombineStaff";
    }
    \translator{ \HaraKiriStaffContext }

    \translator {
      %\ScoreContext
      \OrchestralScoreContext
      \accepts "VoiceCombineStaff";
      \accepts "StaffCombineStaff";
      skipBars = ##t 

      markScriptPadding = #4.0

      BarNumber \override #'padding = #3
      RestCollision \override #'maximum-rest-count = #1
    }
  }
}
