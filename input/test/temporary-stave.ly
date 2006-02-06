
\version "2.7.32"

\header {

texidoc= "
An additional stave can be typeset in the middle of a score line. 
A new context type is created for the temporary staff to avoid printing 
time and key signatures and clef at the beginning of the extra stave.
"
} 

\score {
   {
    \relative c' {
      \context Staff = One { c4 d e f }
      <<
        {c d e f | c d e f | c c c c | c c c c }
        \context TemporaryStaff = Two { c4 c8 d e4 f | c d e f |
          c c c c | c c c8 b c b |}
      >>
      c4 d e f |
    }
  }
  \layout {
    \context {
      \Score
      \consists Span_bar_engraver
      % Avoid a vertical line at the beginning of the system:
      \remove System_start_delimiter_engraver
      % Incorporate the new context in the hierarchy
      \accepts "TemporaryStaff"
    }
    \context {
      \Staff
      \name "TemporaryStaff"
      \alias "Staff"
      \remove "Clef_engraver"
      \remove "Time_signature_engraver"
      \remove "Key_engraver"
    }
  }
}

