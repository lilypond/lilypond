% dynamics collide with staff

\header {
texidoc="Template for part-combining orchestral scores";
}


End = { \skip 1*6; }
violoncello = \notes\relative c'' {
  c1\ff d e \break
  c1\ff d e \break
}

contrabasso = \notes\relative c'' {
  c1\pp d e
  c2\pp c d1 e
}

flautiStaff =  \notes \context VoiceCombineStaff = flauti <
 \context VoiceCombineVoice=oneBassi \End
 \context VoiceCombineVoice=twoBassi \End
  \context VoiceCombineVoice=Flauti \partcombine VoiceCombineVoice
    \context VoiceCombineThread=oneFlauti \violoncello
    \context VoiceCombineThread=twoFlauti \contrabasso
>


\score {
  <
  \flautiStaff

  \context PianoStaff = bassi_group \notes <
    \context StaffCombineStaff=oneBassi \End
    \context StaffCombineStaff=twoBassi \End

    \context StaffCombineStaff=oneBassi \partcombine StaffCombineStaff
      \context StaffCombineVoice=oneBassi \violoncello
      \context StaffCombineVoice=twoBassi \contrabasso
 >

  >
  \paper{
    % \paperSixteen

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
      TimeSignature \override #'style = #'C
      skipBars = ##t 
      BarNumber \override #'padding = #3
      RestCollision \override #'maximum-rest-count = #1
    }
  }
}
