
% this chart is used in the manual too.

\version "2.1.22"
\header { texidoc = "@cindex Script Abbreviations

Some scripts may be entered using an abbreviation.

"
	  
}

\score {
    \notes \context Voice {
      \override TextScript  #'font-family = #'typewriter
      \override TextScript  #'font-shape = #'upright
      c''4-._"c-."      s4
      c''4--_"c-{}-"    s4
      c''4-+_"c-+"      s4
      c''4-|_"c-|"      s4
      c''4->_"c->"      s4
      c''4-^_"c\\^{ }" s4
      c''4-__"c\_" s4      
    }
  }

