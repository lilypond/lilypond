#(ly:set-option 'old-relative)
\version "1.9.1"
% possible rename to bar-lines-foo.  -gp

\header{ texidoc = "@cindex Bar Lines Remove
You can stop LilyPond from printing bar lines by removing the engraver. "
}

\score {
  \notes \relative c'' {
    a b c d
    d c b a
  }
  \paper {
    raggedright = ##t
    \translator {
      \StaffContext
      whichBar = #""
      \remove "Time_signature_engraver"
    }
  }
}


