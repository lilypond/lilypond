\version "2.3.4"



\header {
    texidoc = "Cross voice ties can be faked by using transparent noteheads."
    }


\score {
\context Staff  {
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
