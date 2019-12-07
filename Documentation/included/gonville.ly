\header {

  texidoc = "Use an alternative music font (gonville).  To use this,
put gonville-20.otf and gonville-brace.otf in a directory where
lilypond can find them (use the --include option to extend the search
path)."

}

\version "2.19.21"

\paper {
  myStaffSize = 20

  %% bogus statement, because myStaffSize must be assigned before
  %% the scheme statement starts parsing.
  bogus = 42
  
  #(add-music-fonts fonts "gonville" 'gonville '((20 . 20.0))
     (/ myStaffSize 20))
}

sampleMusic = \relative {
  a'4-\trill_\sfz b8 c16 d32
}


{
  \set Staff.instrumentName = "Default"
  \sampleMusic
}

\score {
  {
    \set Staff.instrumentName = "Feta"
    \sampleMusic
  }
  \layout {
    #(define font-defaults
      '((font-family . feta) (font-encoding . fetaMusic)))
  }
}

\score {
  \relative c'' {
    \set Staff.instrumentName = "Gonv"
    \sampleMusic
  }
  \layout {
    #(define font-defaults
      '((font-family . gonville) (font-encoding . fetaMusic)))
  }
}

\relative
{
  \set Staff.instrumentName = "Gonv override"
  c''2^\trill c
  \override NoteHead.font-family = #'gonville
  \override Script.font-family = #'gonville
  c^\trill c 
}
