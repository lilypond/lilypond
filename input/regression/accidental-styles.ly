\version "2.23.4"

\header {
  texidoc = "Test all available accidental styles."
}

\paper {
  indent = 50
  ragged-right = ##f
}

musicA = {
  <<
    \relative {
      cis''8 fis, bes4 <a cis>8 f bis4 |
      cis2. <c, g'>4 |
    }
    \\
    \relative {
      ais'2 cis, |
      fis8 b a4 cis2 |
    }
  >>
}

musicB = {
  \clef bass
  \new Voice {
    \voiceTwo \relative {
      <fis a cis>8[ <fis a cis>
      \change Staff = up
      cis' cis
      \change Staff = down
      <fis, a> <fis a>]
      \showStaffSwitch
      \change Staff = up
      dis'4 |
      \change Staff = down
      <fis, a cis>4 gis <f a d>2 |
    }
  }
}

testStyle =
#(define-music-function (context-type extra-music style)
   (symbol? (ly:music? *unspecified*) symbol-list?)
   #{
      \new #context-type \with {
        instrumentName =
          \markup \typewriter
            #(string-join
               (map symbol->string style)
               ".")
      }
      {
        <<
          \new Staff = "up" {
            \accidentalStyle #style
            #extra-music
            \musicA
          }
          \new Staff = "down" {
            \accidentalStyle #style
            #extra-music
            \musicB
          }
        >>
      }
   #})

\testStyle PianoStaff default
\testStyle PianoStaff voice
\testStyle PianoStaff modern
\testStyle PianoStaff modern-cautionary
\testStyle PianoStaff modern-voice
\testStyle PianoStaff modern-voice-cautionary
\testStyle PianoStaff piano
\testStyle PianoStaff piano-cautionary
\testStyle ChoirStaff choral
\testStyle ChoirStaff choral-cautionary
\testStyle PianoStaff neo-modern
\testStyle PianoStaff neo-modern-cautionary
\testStyle PianoStaff neo-modern-voice
\testStyle PianoStaff neo-modern-voice-cautionary
\testStyle PianoStaff dodecaphonic
\testStyle PianoStaff dodecaphonic-no-repeat
\testStyle PianoStaff dodecaphonic-first
\testStyle PianoStaff { \key fis \minor }teaching
\testStyle PianoStaff no-reset
\testStyle PianoStaff forget
