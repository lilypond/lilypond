#(ly:set-option 'old-relative)
\version "1.9.4"



\header {
    texidoc = "Cross voice ties can be faked by blanking noteheads."
    }


\score {
\context Staff \notes {
\relative c''  <<
  {
      \once \property Voice.Stem \set #'transparent = ##t
      b8~ b8 }
  \\
  { b[ g8] }
>>
}
\paper { raggedright = ##t }
}
