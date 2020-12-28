\header {

  texidoc = "This file tests various Scheme utility functions." 

}


\version "2.19.22"


%% very platform dependent.
%% #(ly:font-config-display-fonts)

#(ly:progress "~A\n" (ly:duration->string (ly:make-duration 2 2 3/7)))
#(ly:parser-parse-string (ly:parser-clone) "foo  = #1 #(ly:progress \"hello there\n\")")
