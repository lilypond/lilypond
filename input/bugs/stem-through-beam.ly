% strange bug with stems through beams in second beam
	
End = { \skip 1*8; }

violaii = \notes\relative c' {
	[\!f8\sf(\>as f as][f g d)\!g]|
}

violai=\notes\relative c' {
	[\!f8\sf(\>as f as][f g d)\!g]|
}

violeStaff =  \notes \context Staff = viole <
 \context Voice=oneViola <
 		\property Staff.instrument = #"Viola"
		\property Staff.instr = #"Vla."

 \End
 >
 \context Voice=twoViola <
		\property Staff.instrument = #"Viola II"
		\property Staff.instr = #"Vla. II"
 \End
 >
  \context Voice=oneViola \partcombine Voice
    \context Thread=oneViola \violai
    \context Thread=twoViola \violaii
>

\score {
  \violeStaff
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

      % By default, turn off the Thread_devnull_engraver
      % at Voice level
      devNullThread = #'never
      soloText = #"I."
      soloIIText = #"II."

      % Hmm
      currentBarNumber = #218
      BarNumber \override #'padding = #3
      RestCollision \override #'maximum-rest-count = #1
      marginScriptHorizontalAlignment = #1
      TimeSignature \override #'style = #'C
    }
  }
}

