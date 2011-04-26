\version "2.13.61"
%% This should really be 2.15.0, because the glissando code
%% hasn't been backported -- this is a git-only test at this point

\header {

  texidoc = "
A glissando between chords should not interfere with line breaks.  In
this case, the music should be in two lines and there should be no
warning messages issued.  Also, the glissando should be printed.
"

}

theNotes = {
  <c e>4 <c e> <c e>
  \glissando
  d
}

\score {
  \new Staff {
    \relative c'' {
      \theNotes
      \break
      \theNotes
    }
  }
}
