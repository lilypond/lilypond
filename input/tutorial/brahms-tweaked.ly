#(ly:set-option 'old-relative)
\version "1.9.1"
\score {
    \notes\context PianoStaff <
    \context Staff = up
    \relative c'' <
        {   \key d\major 
            \property Voice.Slur \set #'attachment = #'(stem . stem)

	    \once \property Voice.Fingering
	      \override #'extra-offset = #'(-0.3 . -1.8) 
           fis4-3_\p(~
   

            fis16 a)^" "^\markup { \finger "5" } } \\
        {
            \dynamicUp
            \property Voice.DynamicLineSpanner
	      \override #'padding = #5.0
            \tieUp
            fis16(\> d b\! \translator Staff = down
	    \stemUp
	    \clef treble g~ <<g e>>8) } \\
        { s16
	  \once \property Voice.Stem \set #'transparent = ##t
          d'
          ~ <<d b>>4 }
    >
    \context Staff = down {
        \key d \major
        \time 3/8 \clef bass s4. }
    >
    \paper { raggedright = ##t}
}



