\version "2.23.14"

\header {
  lsrtags = "breaks, repeats, symbols-and-glyphs, workaround"

  texidoc = "
If you want to place an exiting segno sign and add text like @qq{D.S. al
Coda} next to it where usually the staff lines are you can use this
snippet. The coda will resume in a new line. There is a variation
documented in this snippet, where the coda will remain on the same
line.
"

  doctitle = "Positioning segno and coda (with line break)"
}


{
  \relative c'' {
    c4 c c c c c c c c c c c
    \repeat segno 2 {
      c4 c c c c c c c
      \alternative {
        \volta 1 {
          c4 c c c c c c c c c c c
          % If you don't use \break at Coda, use \noBreak here
          % and after \bar "" below.
          \noBreak
          \section % double bar line
          \cadenzaOn % pause bar count
          \stopStaff % remove staff lines
          % Increasing the unfold counter will expand the staff-free space
          \repeat unfold 6 {
            s1
            \bar ""
          }
          % Place JumpScript where the staff would normally be.
          \once \override Score.JumpScript.outside-staff-priority = ##f
          \once \override Score.JumpScript.Y-offset = 0
          \startStaff % resume bar count
          \cadenzaOff % show staff lines again
        }
      }
    }
    \sectionLabel "Coda"
    % Show Coda on a new line
    \break
    \repeat unfold 8 { c4 c c c }
    \fine
  }
}
