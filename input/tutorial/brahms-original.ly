#(ly:set-option 'old-relative)
\version "2.3.16"
\score {
    \context PianoStaff <<
    \context Staff = up
    \relative c'' <<
        {   \key d\major 
            fis4-3_\p(~
            fis16 a)-5 } \\
        {
            fis16(\> d b\! \change Staff = down
            \clef treble g~ <g e>8) } \\
        { s16
          d'
          ~ <d b>4 }
    >>
    \context Staff = down {
        \key d \major
        \time 3/8 \clef bass s4. }
    >>
    \paper { raggedright = ##t}
}



