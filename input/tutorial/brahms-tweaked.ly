#(ly:set-option 'old-relative)
\version "2.3.16"
\score {
    \context PianoStaff <<
    \context Staff = up
    \relative c'' <<
        {   \key d\major 
            \override Slur  #'attachment = #'(stem . stem)

	    \once \override Fingering  #'extra-offset = #'(-0.3 . -1.8) 
           fis4-3_\p(~
   

            fis16 a)^" "^\markup { \finger "5" } } \\
        {
            \dynamicUp
            \override DynamicLineSpanner  #'padding = #5.0
            \tieUp
            fis16(\> d b\! \change Staff = down
	    \stemUp
	    \clef treble g~ <g e>8) } \\
        { s16
	  \once \override Stem  #'transparent = ##t
          d'
          ~ <d b>4 }
    >>
    \context Staff = down {
        \key d \major
        \time 3/8 \clef bass s4. }
    >>
    \paper { raggedright = ##t}
}



