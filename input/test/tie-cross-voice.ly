\version "2.1.26"



\header {
    texidoc = "Cross voice ties can be faked by blanking noteheads."
    }


\score {
\context Staff \notes {
\relative c''  <<
  {
      \once \override Stem  #'transparent = ##t
      b8~ b8 }
  \\
  { b[ g8] }
>>
}
\paper { raggedright = ##t }
}
