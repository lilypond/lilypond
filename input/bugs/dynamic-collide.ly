% dynamics should not collide with staff
% dynamics (of two voices) should not collide with eachother
% strange stem through beam bug in bar 8 on viola staff
% 1-staff-high brace should collapse

\header {
texidoc="Template for part-combining orchestral scores";
}


End = { \skip 1*8; }

violaI = \notes\relative c'' {
  c1 \break c c c c\break
	g4\p\< r r r8 g(|
	)c,4 r r r8 c|
	[\!f8\sf(\>as f as][f g d)\!g]|
		r [es'\p(c)es] r [d(bes)d]|


}
violaII = \notes\relative c'' {
  c1 c c c c
	g4\p\< r r r8 g(|
	)c,4 r r r8 c|
	[\!f8\sf(\>as f as][f g d)\!g]|
	r [c\p(g)c] r [bes(g)bes]|
}


violoncello = \notes\relative c {
  c4\< c c c 
  \!c1\pp c c c
	\property Voice.crescendoText = #"cresc."
	\property Voice.crescendoSpanner = #'dashed-line
  [g8\p\<(bes' g bes][d bes g)d~]|
  [d8(a' fis a][d a fis)d]|
  [\!f!8\sf\>(as f as][f g d)g]|
  %75
  \!c,4\p r d r|
}

contrabasso = \notes\relative c {
  d8\< d d d  d d d d 
  \!c1\ff c c c
	\property Voice.crescendoText = #"cresc."
	\property Voice.crescendoSpanner = #'dashed-line
  g4\p\< r r r8 g(|
  )c4 r r r8 c(|
  \!)b1\sf|
  c4\p r d r|
}

violeStaff =  \notes \context Staff = viole <
 \context Voice=oneViole <
 		\property Staff.instrument = #"Viola"
		\property Staff.instr = #"Vla."

 \End
 >
 \context Voice=twoViole <
		\property Staff.instrument = #"Viola II"
		\property Staff.instr = #"Vla. II"
 \End
 >
  \context Voice=oneViole \partcombine Voice
    \context Thread=oneViole \violaI
    \context Thread=twoViole \violaII
>

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
  \context StaffGroup <
  \violeStaff

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

