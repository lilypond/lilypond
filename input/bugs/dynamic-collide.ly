
% dynamics should not collide with staff
% dynamics (of two voices) should not collide with eachother

\header {
texidoc="Template for part-combining orchestral scores";
}


End = { \skip 1; }
violoncello = \notes\relative c'' {
  c1\ff 
}

contrabasso = \notes\relative c'' {
  c1\pp 
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
  }
}

