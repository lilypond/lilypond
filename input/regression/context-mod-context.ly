\version "2.19.21"

\header  {
texidoc = "Context modifications can be stored into a variable as a
\with object. They can be later inserted directly into a context definition."
}

% Some sample modifications to be inserted into a \with block later on
ctxmod = \with {
  \remove "Time_signature_engraver"
  \consists "Ambitus_engraver"
  \override StaffSymbol.line-count = 4
}

music = \relative { \key fis \minor c''1 d e }

\score { <<
    \new Staff { \music}
  >>
  \layout {
    \context { \Staff
      \ctxmod
      \override NoteHead.style = #'petrucci
    }
  }
}


\score { <<
    \new Staff { \music}
  >>
  \layout {
    \context { \Staff
      \override StaffSymbol.line-count = 3
      \override NoteHead.style = #'petrucci
    }
    % Should override the above definitions, but not reset others
    \context { \Staff
      \ctxmod
    }
  }
}

