\header {

  texidoc = "The default music font can be overridden by setting
  @code{(font-family . gonville)} in the @code{font-defaults} in the
  paper block."

}

\version "2.13.6"

\paper {
  % Load the alternative font definitions into the font tree.
  #(add-music-fonts fonts "gonville" 'gonville (/ staff-height (* 20 pt)))
}

%{
  Note: for this to work, do something like

     wget -P out http://www.chiark.greenend.org.uk/~sgtatham/gonville/gonville-r8724.tar.gz
     tar xzf out/gonville-r8724.tar.gz
     ln out/gonville-r8724/lilyfonts/otf/gonville* out/share/lilypond/current/fonts/otf/
     (cd out/share/lilypond/current/fonts/otf && for i in gonville*; do mv $i $(echo $i | sed s/gonville/gonville-/); done)


  To show all available fonts, do

     lilypond -dshow-available-fonts blabla

%}

\relative c'' {
  \set Staff.instrumentName = #"Default"
  a4-\trill b8 c16 d32
}

\score {
  \relative c'' {
    \set Staff.instrumentName = #"Feta"
    a4-\trill b8
    c16 d32
  }
  \layout {
    #(define font-defaults
      '((font-family . feta) (font-encoding . fetaMusic)))
  }
}

\score {
  \relative c'' {
    \set Staff.instrumentName = #"Gonville"
    a4-\trill b8 c16 d32
  }
  \layout {
    #(define font-defaults
      '((font-family . gonville) (font-encoding . fetaMusic)))
  }
}
