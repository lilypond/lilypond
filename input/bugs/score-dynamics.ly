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

flautiStaff =  \notes \context Staff = flauti <
 \context Voice=oneBassi \End
 \context Voice=twoBassi \End
  \context Voice=Flauti \partcombine Voice
    \context Thread=oneFlauti \violoncello
    \context Thread=twoFlauti \contrabasso
>

staffCombineProperties = {
	\property Voice.devNullThread = #'unisolo
	\property Voice.soloADue = ##t
	\property Voice.soloText = #""
	\property Voice.soloIIText = #""
	% This is non-conventional, but currently it is
	% the only way to tell the difference.
	\property Voice.aDueText = #"\\`a2"
	\property Voice.splitInterval = #'(1 . 0)
	\property Voice.changeMoment = #`(,(make-moment 1 1) . ,(make-moment 1 1))
}

\score {
  <
  \flautiStaff

  \context PianoStaff = bassi_group \notes <
    \context Staff=oneBassi \End
    \context Staff=twoBassi \End

    \context Staff=oneBassi \partcombine Staff
      \context Voice=oneBassi { \staffCombineProperties \violoncello }
      \context Voice=twoBassi { \staffCombineProperties \contrabasso }
 >

 >
  \paper {
    % \paperSixteen
    linewidth = 80 * \staffspace;
    textheight = 200 * \staffspace;
    \translator{
      \ThreadContext
      \consists "Rest_engraver";
    }
    \translator{
      \VoiceContext
      \remove "Rest_engraver";    

      % The staff combine (bassi part) needs a
      % thread_devnull_engraver here.
      % Instead of maintaining two separate hierarchies,
      % we switch add it, but switch it off immideately.
      % The staff combine parts switch it on.
      devNullThread = #'never
      \consists "Thread_devnull_engraver";

      soloText = #"I."
      soloIIText = #"II."
      soloADue = ##f
    }
    \translator{
      \HaraKiriStaffContext
      \consists "Mark_engraver";
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

