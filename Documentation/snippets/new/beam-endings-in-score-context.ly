\version "2.13.4"

\header {
  lsrtags = "rhythms"
  texidoc = "
Beam-ending rules specified in the @code{Score} context apply to all
staves, but can be modified at both @code{Staff} and @code{Voice}
levels:
"
  doctitle = "Beam endings in Score context"
}

\relative c'' {
  \time 5/4
  % Set default beaming for all staves
  \overrideBeamSettings #'Score #'(5 . 4) #'end
     #'(((1 . 8) . (3 4 3))
        ((1 . 16) . (6 8 6))
        ((1 . 32) . (12 16 12)))
  <<
    \new Staff {
      c8 c c c c c c c c c
    }
    \new Staff {
      % Modify beaming for just this staff
      \overrideBeamSettings #'Staff #'(5 . 4) #'end
        #'((* . (3 2)))
      c8 c c c c c c c c c
    }
    \new Staff {
      % Inherit beaming from Score context
      <<
        {
          \voiceOne
          c8 c c c c c c c c c
        }
        % Modify beaming for this voice only
        \new Voice {
          \voiceTwo
          \overrideBeamSettings #'Voice #'(5 . 4) #'end
              #'((* . (3 2)))
          a8 a a a a a a a a a
        }
      >>
    }
  >>
}
