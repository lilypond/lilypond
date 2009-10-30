\version "2.13.6"

#(set-global-staff-size 22.45)

% TODO
%{
- you might want to add a custom #'gap = #0.5 and #'extra-offset to
  glissandi that go to an accidental'd note.  You might want to
  do a similar thing, because I see collisions.

- the fermata seems awfully high
- why are there so many clashing notehead warnings?
%}

%% Hide fret number: useful to draw slide into/from a casual point of
%% the fretboard.
hideFretNumber = { \once \override TabNoteHead #'transparent = ##t 
                 \once \override NoteHead #'transparent = ##t 
                 \once \override Stem #'transparent = ##t
                 \once \override NoteHead #'no-ledgers = ##t 
}

\paper {
  indent= #0
  line-width= #180
}

upper=  \relative c' {
  \time 4/4 
  \key e \major
  \set Staff.midiInstrument = #"acoustic guitar (steel)"
  \set fingeringOrientations = #'(left)
  
  %\override Staff.Glissando #'extra-offset = #' (0.0 . 1.0)
  \partial 4. \acciaccatura c16 \glissando cis8  e4 
  < cis-1 g'-3 >2 s8 \grace a16 ( \glissando <b-2>8\3 )  <d-1> ( b ) |
  <e-3>\2 ( <d-1> b ) \grace <ais-2>16 ( \glissando  a8  g ) s4. |
  s4.  < d'\3 g\2 >8  < gis,\4  d'\3 fis\2 >2\arpeggio ~ |
  
  < gis\4  d'\3 fis\2 >2  < b'\2\harmonic e\harmonic >2^\markup { \musicglyph #"scripts.ufermata" } |
  
}

lower=  \relative c {
  \set fingeringOrientations = #'(left)
  
  \partial 4. s4. |
  s4  e,4  s2 |
  s2 s8 <e'-3>4. ~ |
  e4  \hideFretNumber \grace { b8 \glissando s4 }  <e-2>4\5  e,2 ~ |
  
  e2  < e'\6\harmonic > |
}

\score {
  \new StaffGroup <<
    \new Staff = "guitar" <<
      \context Voice = "upper guitar" { \clef "G_8" \voiceOne \upper }
      \context Voice = "lower guitar" { \clef "G_8" \voiceTwo \lower }
    >>
    \new TabStaff = "tab" <<
      \context TabVoice = "upper tab" { \clef "moderntab" \voiceOne \upper }
      \context TabVoice = "lower tab" { \clef "moderntab" \voiceTwo \lower }
    >>
  >>
  
 \midi {
    \context {
      \Score tempoWholesPerMinute = #(ly:make-moment 120 4)
    }
  }
  
 \layout {
    \context {
    \Staff
    \override StringNumber #'transparent = ##t
    }
    
    \context {
    \TabStaff
    \revert Arpeggio #'stencil
    }
  }
}
