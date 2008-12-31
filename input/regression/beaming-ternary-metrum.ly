
\version "2.12.0"

\header {

  texidoc = "Automatic beaming works also in ternary time sigs. In
  this case, the 8th is a beat, so the 16ths are split into two
  groups.  This can be avoided by overriding @code{beatLength} to be
  three 8th notes."

}

\layout { ragged-right = ##t}

\relative c'' {
  \time 6/8
  c8.[ c16 c16 c16] 
  \set beatLength = #(ly:make-moment 3 8)
  c8.[ c16 c16 c16] 
}

