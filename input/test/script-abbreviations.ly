\version "1.7.18"
% FIXMEsoon: check if test/ is the right place.
\header { texidoc = "@cindex Script Abbreviations
Some scripts may be entered using an abbreviation. "
}

\score {
    \notes \context Voice {
      \property Voice.TextScript \set #'font-family = #'typewriter
      \property Voice.TextScript \set #'font-shape = #'upright
      c''4-._"c-."      s4
      c''4--_"c-{}-"    s4
      c''4-+_"c-+"      s4
      c''4-|_"c-|"      s4
      c''4->_"c->"      s4
      c''4-^_"c-\\^{ }" s4
      c''4-__"c-\_" s4      
    }
  }

