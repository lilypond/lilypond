% on their own staff, voices should be stemboth (not up/down)

End = { \skip 1*8; }

violoncello = \notes\relative c' {
   c8 c c c    c8 c c c\break
}

contrabasso = \notes\relative c {
   c4 c8 c    c8 c c c\break
}

staffCombinePianoStaffProperties = {
	\property PianoStaff.devNullThread = #'()
	\property PianoStaff.soloADue = ##t
	\property PianoStaff.soloText = #""
	\property PianoStaff.soloIIText = #""
	% This is non-conventional, but currently it is
	% the only way to tell the difference.
	\property PianoStaff.aDueText = #"\\`a2"
	\property PianoStaff.splitInterval = #'(1 . 0)
	\property PianoStaff.changeMoment = #`(,(make-moment 1 1) . ,(make-moment 1 1))
}


\score {
  <
  \context PianoStaff = bassi_group \notes <
    \staffCombinePianoStaffProperties
    \context Staff=oneBassi < \clef bass;
    		\property Staff.instrument = #'(lines
    		  "Violoncello" "    e" "Contrabasso")

    		\property Staff.instr = #"Vc."
		\End >
    \context Staff=twoBassi < \clef bass;
    		\property Staff.instrument = #"Contrabasso"
		\property Staff.instr = #"Cb."

    \End >
  
    \context Staff=oneBassi \partcombine Staff
      \context Voice=oneBassi { \violoncello }
      \context Voice=twoBassi { \contrabasso }
 >
>
  \paper {
    % \paperSixteen
    linewidth = 80 * \staffspace;
    textheight = 200 * \staffspace;
    \translator{
      \ThreadContext
      \consists "Rest_engraver";
      
      % Set value for engraver at thread level,
      % to override the default that is set in ScoreContext
      % for added engraver at Voice level
      devNullThread = #'()
    }
    \translator{
      \VoiceContext
      \remove "Rest_engraver";    

      % The staff combine (bassi part) needs a
      % thread_devnull_engraver here.
      % Instead of maintaining two separate hierarchies,
      % we switch add it, but switch it off immideately.
      % The staff combine parts switch it on.
      % devNullThread = #'never
      \consists "Thread_devnull_engraver";
    }
    \translator{
      \HaraKiriStaffContext
      \consists "Mark_engraver";
    }
    \translator {
      \OrchestralScoreContext
      skipBars = ##t
      soloText = #"I."
      soloIIText = #"II."
      % By default, turn off the Thread_devnull_engraver
      % at Voice level
      devNullThread = #'never

      % Hmm
      currentBarNumber = #218
      BarNumber \override #'padding = #3
      RestCollision \override #'maximum-rest-count = #1
      marginScriptHorizontalAlignment = #1
      TimeSignature \override #'style = #'C
    }
  }
}

