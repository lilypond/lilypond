\version "1.5.68"
\score {
    \notes\context PianoStaff <
    \context Staff = up
    \relative c'' <
        {   \key d\major 
            \property Voice.Slur \set #'attachment = #'(stem . stem)

            \property Voice.Fingering \set #'extra-offset = #'(-0.3 . -1.8) 
            fis4-3_\p ( ~

            \property Voice.Fingering \revert #'extra-offset
            fis16 )a^" "^#'(finger "5") } \\
        {
            \dynamicUp
            \property Voice.DynamicLineSpanner \override #'padding = #5.0
            \tieUp
            fis16( \> d \! b \translator Staff = down
	    \stemUp
	    \clef treble g ~ < g8 )e> } \\
        { s16
          \property Voice.Stem \set #'transparent = ##t
          d'
          \property Voice.Stem \revert #'transparent
          ~ < d4 b4  > }
    >
    \context Staff = down {
        \key d \major
        \time 3/8 \clef bass s4. }
    >
    \paper { linewidth = -1. }
}

