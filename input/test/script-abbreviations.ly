\version "1.7.16"

\header {
    texidoc = "Some scripts may be entered using an abbreviation"
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
%% new-chords-done %%
